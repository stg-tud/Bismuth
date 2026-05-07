package webapps.ex2026niowebsocket

import channels.*
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import channels.webrtc.{SessionDescription, WebRTCConnection, WebRTCConnectionFailed, WebRTCConnector}
import de.rmgk.delay.Async
import org.scalajs.dom
import rdts.base.Uid
import replication.BroadcastIO
import replication.research.OverlayNetworkProtocol.DemoState
import replication.research.{OverlayDemoNode, SignalingClient, SignalingServer}

import scala.collection.mutable
import scala.scalajs.js
import scala.util.{Failure, Success}

object OverlayNetworkGraphNetworking {

  type Envelope = BroadcastIO.Envelope[DemoState]

  def createRtcConnector(): WebRTCConnector =
    WebRTCConnector(new dom.RTCConfiguration {
      iceServers = js.Array[dom.RTCIceServer](new dom.RTCIceServer {
        urls = js.Array[String]("stun:stun.t-online.de:3478")
      })
    })

  private def installPeerFailurePropagation(connector: WebRTCConnector, channel: dom.RTCDataChannel): Unit = {
    def connectionStateString: String =
      try connector.peerConnection.asInstanceOf[js.Dynamic].connectionState.asInstanceOf[String]
      catch case _: Throwable => "<unsupported>"

    def closeChannel(): Unit =
      if channel.readyState != dom.RTCDataChannelState.closed && channel.readyState != dom.RTCDataChannelState.closing
      then channel.close()

    var pollHandle = 0
    pollHandle = dom.window.setInterval(
      () =>
        if channel.readyState != dom.RTCDataChannelState.connecting then dom.window.clearInterval(pollHandle),
      1000
    )

    channel.addEventListener("open", (_: dom.Event) => dom.window.clearInterval(pollHandle))
    channel.addEventListener("close", (_: dom.Event) => dom.window.clearInterval(pollHandle))
    channel.addEventListener("error", (_: dom.Event) => ())

    connector.peerConnection.addEventListener(
      "iceconnectionstatechange",
      (_: dom.Event) => {
        val state = connector.peerConnection.iceConnectionState
        state match
            case dom.RTCIceConnectionState.failed       => closeChannel()
            case dom.RTCIceConnectionState.disconnected => closeChannel()
            case dom.RTCIceConnectionState.closed       => closeChannel()
            case _                                      => ()
      }
    )

    connector.peerConnection.addEventListener("signalingstatechange", (_: dom.Event) => ())
    connector.peerConnection.addEventListener("icegatheringstatechange", (_: dom.Event) => ())

    connector.peerConnection.addEventListener(
      "connectionstatechange",
      (_: dom.Event) => {
        val state = connectionStateString
        state match
            case "failed" | "disconnected" | "closed" => closeChannel()
            case _                                    => ()
      }
    )
  }

  def createOverlayDataChannel(connector: WebRTCConnector): dom.RTCDataChannel = {
    val channel = connector.peerConnection.createDataChannel(
      "overlay-webrtc",
      new dom.RTCDataChannelInit {
        negotiated = true
        id = 23
      }
    )
    installPeerFailurePropagation(connector, channel)
    channel
  }

  final class WebRtcSignalingBridge(
      url: String,
      topic: String,
      node: OverlayDemoNode,
      selfDetails: Set[ChannelConnectInfo],
      initialSeed: Option[Uid],
      topicLookupCount: Int,
      onRegistered: () => Unit,
  ) {
    private def logFailure(context: String, err: Throwable): Unit =
      println(s"[overlay-signaling] $context: ${err.getClass.getSimpleName}: ${Option(err.getMessage).getOrElse("")}")

    private case class OutgoingAttempt(
        connector: WebRTCConnector,
        channel: dom.RTCDataChannel,
        timeoutHandle: Int,
        fail: Throwable => Unit,
    )

    private val wsResolver                            = new WebSocketConnectionDetailsResolver
    private val outgoing                              = mutable.Map.empty[Uid, OutgoingAttempt]
    private var rediscoveryHandle: js.UndefOr[js.Any] = js.undefined
    private var lastLookupAtMillis                    = 0.0
    private val answerTimeoutMillis                   = 15000

    private def lookupNow(): Unit = {
      lastLookupAtMillis = dom.window.performance.now()
      client.lookupTopic(topic, topicLookupCount).run {
        case Success(_)   => ()
        case Failure(err) => logFailure("lookupTopic failed", err)
      }
      initialSeed.foreach(uid =>
        client.lookupPeer(uid).run {
          case Success(_)   => ()
          case Failure(err) => logFailure(s"lookupPeer failed uid=${Uid.unwrap(uid)}", err)
        }
      )
    }

    private def maybeRediscover(): Unit = {
      val now      = dom.window.performance.now()
      val isolated = node.activeView.isEmpty && node.passiveView.isEmpty
      if client.isConnected && isolated && now - lastLookupAtMillis >= 3000 then lookupNow()
    }

    private lazy val client: SignalingClient = new SignalingClient(
      server = ChannelConnectInfo.WebSocket(url),
      resolver = wsResolver,
      localUid = node.localUid.uid,
      initialAnnouncements = Map(topic -> selfDetails),
      onRegistered = () => {
        lookupNow()
        onRegistered()
      },
      onPeerInfo = (uid, topics) =>
        topics.values.foreach { descriptors =>
          if uid != node.localUid.uid && descriptors.nonEmpty then
              node.discoverPeers(PeerConnectInfo(uid, descriptors) :: Nil)
        },
      onTopicInfo = (_, peers) =>
        node.discoverPeers(peers.iterator.collect {
          case (uid, descriptors) if uid != node.localUid.uid && descriptors.nonEmpty =>
            PeerConnectInfo(uid, descriptors)
        }.toList),
      onOffer = (from, session) => handleIncomingOffer(from, session),
      onAnswer = (from, session) => handleIncomingAnswer(from, session),
    )

    private def handleIncomingOffer(from: Uid, session: SignalingServer.Session): Unit = {
      val connector  = createRtcConnector()
      val channel    = createOverlayDataChannel(connector)
      var sentAnswer = false
      node.addOverlayConnection(WebRTCConnection.openLatent(channel))
      connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
        case Success(_) =>
          connector.lifecycle.run {
            case Success(overview) if !sentAnswer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
              overview.localSession.foreach { answer =>
                sentAnswer = true
                client.answer(from, SignalingServer.Session(answer.descType, answer.sdp)).run {
                  case Success(_)   => ()
                  case Failure(err) => logFailure(s"answer failed to ${Uid.unwrap(from)}", err)
                }
              }
            case Success(_)   => ()
            case Failure(err) => logFailure(s"incoming offer lifecycle failed from ${Uid.unwrap(from)}", err)
          }
        case Failure(err) => logFailure(s"updateRemoteDescription failed from ${Uid.unwrap(from)}", err)
      }
    }

    private def handleIncomingAnswer(from: Uid, session: SignalingServer.Session): Unit =
      outgoing.remove(from).foreach { attempt =>
        dom.window.clearTimeout(attempt.timeoutHandle)
        attempt.connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
          case Success(_)   => ()
          case Failure(err) =>
            attempt.channel.close()
            attempt.connector.peerConnection.close()
            logFailure(s"apply answer failed from ${Uid.unwrap(from)}", err)
        }
      }

    def start(): Unit = {
      client.start()
      rediscoveryHandle = dom.window.setInterval(() => maybeRediscover(), 1000)
    }

    def stop(): Unit = {
      rediscoveryHandle.foreach(handle => dom.window.clearInterval(handle.asInstanceOf[Int]))
      rediscoveryHandle = js.undefined
      outgoing.values.foreach { attempt =>
        dom.window.clearTimeout(attempt.timeoutHandle)
        attempt.channel.close()
        attempt.connector.peerConnection.close()
      }
      outgoing.clear()
      client.stop()
    }

    def canConnect(details: ChannelConnectInfo): Boolean =
      details match
          case ChannelConnectInfo.WebRtc(peerId) => peerId != Uid.unwrap(node.localUid.uid) && client.isConnected
          case _                                 => false

    def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] = details match
        case ChannelConnectInfo.WebRtc(peerId) if client.isConnected && peerId != Uid.unwrap(node.localUid.uid) =>
          val target = Uid.predefined(peerId)
          Some(new LatentConnection {
            override def prepare(receiver: Receive): Async[Abort, Connection] = {
              Async.fromCallback { abort ?=>
                val connector                         = createRtcConnector()
                val channel                           = createOverlayDataChannel(connector)
                var sentOffer                         = false
                def failAttempt(err: Throwable): Unit = {
                  outgoing.remove(target).foreach(a => dom.window.clearTimeout(a.timeoutHandle))
                  try channel.close()
                  catch case _: Throwable => ()
                  try connector.peerConnection.close()
                  catch case _: Throwable => ()
                  Async.handler.fail(err)
                }
                WebRTCConnection.openLatent(channel).prepare(receiver).runIn(abort) {
                  case Success(conn) =>
                    Async.handler.succeed(conn)
                  case Failure(err) =>
                    failAttempt(err)
                }
                connector.smartUpdateLocalDescription.run {
                  case Success(_) =>
                    connector.lifecycle.run {
                      case Success(overview)
                          if !sentOffer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
                        overview.localSession match
                            case Some(offer) =>
                              sentOffer = true
                              val timeoutHandle = dom.window.setTimeout(
                                () =>
                                  failAttempt(
                                    WebRTCConnectionFailed(s"timed out waiting for answer from ${Uid.unwrap(target)}")
                                  ),
                                answerTimeoutMillis
                              )
                              outgoing.update(target, OutgoingAttempt(connector, channel, timeoutHandle, failAttempt))
                              client.offer(target, SignalingServer.Session(offer.descType, offer.sdp)).run {
                                case Success(_)   => ()
                                case Failure(err) => failAttempt(err)
                              }
                            case None =>
                              failAttempt(
                                IllegalStateException("missing local webrtc offer after ice gathering completed")
                              )
                      case Success(_)   => ()
                      case Failure(err) => failAttempt(err)
                    }
                  case Failure(err) => failAttempt(err)
                }
              }
            }
          })
        case _ => None
  }
}
