package webapps.ex2026niowebsocket

import channels.*
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import de.rmgk.delay.Async
import channels.webrtc.{SessionDescription, WebRTCConnection, WebRTCConnector}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import org.scalajs.dom
import rdts.base.Uid
import replication.overlay.HyParViewMultiplexed
import replication.research.{OverlayDemoNode, SignalingClient, SignalingServer}
import replication.research.OverlayNetworkProtocol.DemoState
import replication.research.SignalingServer.Message

import scala.collection.mutable
import scala.scalajs.js
import scala.util.{Failure, Success}

object OverlayNetworkGraphNetworking {

  type Envelope = HyParViewMultiplexed.Envelope[DemoState]

  def envelopeConnection(latent: LatentConnection[MessageBuffer])(using JsonValueCodec[Envelope]): LatentConnection[Envelope] =
    LatentConnection.adapt[MessageBuffer, Envelope](
      mb => readFromArray[Envelope](mb.asArray),
      msg => ArrayMessageBuffer(writeToArray(msg)),
      "webrtc-envelope-json"
    )(latent)

  def createRtcConnector(): WebRTCConnector =
    WebRTCConnector(new dom.RTCConfiguration {
      iceServers = js.Array[dom.RTCIceServer](new dom.RTCIceServer {
        urls = js.Array[String]("stun:stun.t-online.de:3478")
      })
    })

  def createOverlayDataChannel(connector: WebRTCConnector): dom.RTCDataChannel =
    connector.peerConnection.createDataChannel(
      "overlay-webrtc",
      new dom.RTCDataChannelInit {
        negotiated = true
        id = 23
      }
    )

  final class WebRtcSignalingBridge(
      url: String,
      topic: String,
      node: OverlayDemoNode,
      selfDetails: Set[ChannelConnectDescriptor],
      initialSeed: Option[Uid],
      topicLookupCount: Int,
      onRegistered: () => Unit,
  )(using JsonValueCodec[Envelope], JsonValueCodec[Message]) {
    private val wsResolver = new WebSocketConnectionDetailsResolver[Message]
    private val outgoing   = mutable.Map.empty[Uid, WebRTCConnector]

    private lazy val client: SignalingClient = new SignalingClient(
      server = ChannelConnectDescriptor.WebSocket(url),
      resolver = wsResolver,
      localUid = node.localUid.uid,
      initialAnnouncements = Map(topic -> selfDetails),
      onRegistered = () => {
        client.lookupTopic(topic, topicLookupCount)
        initialSeed.foreach(client.lookupPeer)
        onRegistered()
      },
      onPeerInfo = (uid, topics) =>
        topics.values.foreach { descriptors =>
          if uid != node.localUid.uid && descriptors.nonEmpty then
            node.discoverPeers(HyParViewMultiplexed.PeerRef(uid, descriptors) :: Nil)
        },
      onTopicInfo = (_, peers) =>
        node.discoverPeers(peers.iterator.collect {
          case (uid, descriptors) if uid != node.localUid.uid && descriptors.nonEmpty =>
            HyParViewMultiplexed.PeerRef(uid, descriptors)
        }.toList),
      onOffer = (from, session) => handleIncomingOffer(from, session),
      onAnswer = (from, session) => handleIncomingAnswer(from, session),
    )

    private def handleIncomingOffer(from: Uid, session: SignalingServer.Session): Unit = {
      val connector  = createRtcConnector()
      val channel    = createOverlayDataChannel(connector)
      var sentAnswer = false
      node.addOverlayConnection(envelopeConnection(WebRTCConnection.openLatent(channel)))
      connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
        case Success(_) =>
          connector.lifecycle.run {
            case Success(overview) if !sentAnswer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
              overview.localSession.foreach { answer =>
                sentAnswer = true
                client.answer(from, SignalingServer.Session(answer.descType, answer.sdp))
              }
            case Success(_)   => ()
            case Failure(err) => err.printStackTrace()
          }
        case Failure(err) => err.printStackTrace()
      }
    }

    private def handleIncomingAnswer(from: Uid, session: SignalingServer.Session): Unit =
      outgoing.remove(from).foreach { connector =>
        connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
          case Success(_)   => ()
          case Failure(err) => err.printStackTrace()
        }
      }

    def start(): Unit = client.start()

    def canConnect(details: ChannelConnectDescriptor): Boolean =
      details match
        case ChannelConnectDescriptor.WebRtc(peerId) => peerId != Uid.unwrap(node.localUid.uid) && client.isConnected
        case _                                       => false

    def connect(details: ChannelConnectDescriptor, label: String): Option[LatentConnection[Envelope]] = details match
      case ChannelConnectDescriptor.WebRtc(peerId) if client.isConnected && peerId != Uid.unwrap(node.localUid.uid) =>
        val target = Uid.predefined(peerId)
        Some(new LatentConnection[Envelope] {
          override def prepare(receiver: Receive[Envelope]): Async[Abort, Connection[Envelope]] =
            Async.fromCallback { abort ?=>
              val connector = createRtcConnector()
              val channel   = createOverlayDataChannel(connector)
              var sentOffer = false
              outgoing.update(target, connector)
              envelopeConnection(WebRTCConnection.openLatent(channel)).prepare(receiver).runIn(abort) {
                case Success(conn) =>
                  Async.handler.succeed(conn)
                case Failure(err) =>
                  outgoing.remove(target)
                  Async.handler.fail(err)
              }
              connector.smartUpdateLocalDescription.run {
                case Success(_) =>
                  connector.lifecycle.run {
                    case Success(overview) if !sentOffer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
                      overview.localSession match
                        case Some(offer) =>
                          sentOffer = true
                          client.offer(target, SignalingServer.Session(offer.descType, offer.sdp))
                        case None =>
                          outgoing.remove(target)
                          Async.handler.fail(IllegalStateException("missing local webrtc offer after ice gathering completed"))
                    case Success(_)   => ()
                    case Failure(err) =>
                      outgoing.remove(target)
                      Async.handler.fail(err)
                  }
                case Failure(err) =>
                  outgoing.remove(target)
                  Async.handler.fail(err)
              }
            }
        })
      case _ => None
  }
}
