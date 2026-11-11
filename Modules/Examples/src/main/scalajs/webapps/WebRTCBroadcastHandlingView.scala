package webapps

import channels.MessagingIO
import channels.broadcastchannel.BroadcastChannelConnector
import channels.connection.{Abort, ByteBufferMessageBuffer, MessageBuffer}
import channels.webrtc.{ConnectorOverview, SessionDescription, WebRTCConnection, WebRTCConnector}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import org.scalajs.dom
import org.scalajs.dom.*
import org.scalajs.dom.html.Table
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.Date
import scala.util.{Failure, Random, Success}

sealed trait BroadcastCommunication
object BroadcastCommunication {
  case class Hello(id: Long)                                                        extends BroadcastCommunication
  case class Request(from: Long, to: Long, sessionDescription: SessionDescription)  extends BroadcastCommunication
  case class Response(from: Long, to: Long, sessionDescription: SessionDescription) extends BroadcastCommunication
}

given converterRead[T](using JsonValueCodec[T]): Conversion[MessageBuffer, T] =
  mb => readFromByteBuffer[T](mb.asByteBuffer)
given converterWrite[T](using JsonValueCodec[T]): Conversion[T, MessageBuffer] =
  v => ByteBufferMessageBuffer(writeToArray[T](v))

given JsonValueCodec[BroadcastCommunication] = JsonCodecMaker.make

/** `offerChannel` is the broadcast channel used to automatically connect to other tabs of the same browser.
  * `defaultRoom` is the initial room for connecting to other browsers via the public PeerJS signaling server.
  */
class WebRTCConnectionView(
    val dataManager: MessagingIO[?],
    offerChannel: String = "channels local broadcast for webrtc offers",
    defaultRoom: String = "default",
) {

  // label seems mostly for auto negotiation
  val channelLabel = "webrtc-channel"
  // id is used for pre negotiated channels
  val channelId: Double = 4

  def example(): Tag = {
    val renderedConnectionTable = table(
      tr(
        th("local session description"),
        th("remote session description"),
        th("gather"),
        th("con"),
        th("signal"),
        th("trickle"),
      )
    ).render

    val renderedAddConnectionButton = button("new peerConnection").render

    Async.fromCallback {
      renderedAddConnectionButton.onclick = (_: MouseEvent) =>
        Async.handler.succeed(())
    }.map: _ =>
        val handling = WebRTCHandling(None)
        renderedConnectionTable.appendChild(handling.controlRow().render)
        addDataChannel(handling)
        handling.peer.smartUpdateLocalDescription.run(errorReporter)
    .runIn(ExecutionContext.global)(errorReporter)

    useLocalBroadcastChannel(renderedConnectionTable)

    val roomInput  = input(placeholder := "room", value := defaultRoom).render
    val roomStatus = span("not connected to any room").render
    val joinButton = button("join room").render
    var leave: () => Unit = () => ()
    joinButton.onclick = (_: MouseEvent) => {
      leave()
      leave = joinRoom(roomInput.value, renderedConnectionTable, roomStatus.textContent = _)
    }

    val para =
      section(
        hr(),
        div(roomInput, joinButton, " ", roomStatus),
        renderedConnectionTable,
        renderedAddConnectionButton
      )

    para
  }

  private def useLocalBroadcastChannel(renderedConnectionTable: Table) = {
    val connector = BroadcastChannelConnector.named(offerChannel)

    var autoconnections: Map[Long, WebRTCHandling] = Map.empty
    val selfId                                     = Random.nextLong()

    connector.connect { conn =>
      {
        case Failure(ex)  => errorReporter.fail(ex)
        case Success(msg) =>
          val communication: BroadcastCommunication = converterRead[BroadcastCommunication].convert(msg)
          Async[Abort].fromCallback {
            communication match
                case BroadcastCommunication.Hello(id) =>
                  val handling = WebRTCHandling(Some {
                    case Success(sd) =>
                      conn.send(BroadcastCommunication.Request(selfId, id, sd).convert).run(Async.handler)
                    case Failure(ex) => Async.handler.fail(ex)
                  })
                  autoconnections = autoconnections.updated(id, handling)
                  renderedConnectionTable.appendChild(handling.controlRow().render)
                  addDataChannel(handling)
                  // pre-negotiated channels do not trigger `negotiationneeded`, so create the offer explicitly
                  handling.peer.smartUpdateLocalDescription.run(Async.handler)

                case BroadcastCommunication.Request(from, `selfId`, sessionDescription) =>
                  val handling = WebRTCHandling(Some {
                    case Success(sd) =>
                      conn.send(BroadcastCommunication.Response(selfId, from, sd).convert).run(Async.handler)
                    case Failure(ex) => Async.handler.fail(ex)
                  })
                  autoconnections = autoconnections.updated(from, handling)
                  renderedConnectionTable.appendChild(handling.controlRow().render)
                  addDataChannel(handling)
                  handling.peer.updateRemoteDescription(sessionDescription).run(Async.handler)
                case BroadcastCommunication.Response(from, `selfId`, sessionDescription) =>
                  autoconnections.get(from).foreach: handling =>
                      handling.peer.updateRemoteDescription(sessionDescription).run(Async.handler)

                // ignore messages to other peers
                case BroadcastCommunication.Request(from, to, desc)  =>
                case BroadcastCommunication.Response(from, to, desc) =>
          }.runIn(Abort())(errorReporter)
      }
    }.runIn(Abort()) {
      case Success(conn) =>
        conn.send(BroadcastCommunication.Hello(selfId).convert).run(errorReporter)
      case Failure(ex) => errorReporter.fail(ex)
    }
  }

  /** Connect to other browsers in `room` using the public PeerJS signaling server.
    * The PeerJS server does not tell us about other peers, so the first peer claims a well known id derived from the room
    * (becoming the hub), and all others connect to the hub (the broadcast protocol relays through the hub).
    * If the connection to the hub fails, peers try to become the hub themselves.
    * Returns a function to leave the room.
    */
  private def joinRoom(room: String, renderedConnectionTable: Table, status: String => Unit): () => Unit = {
    // ids on the public PeerJS server are global, so use a namespace unlikely to be used by anyone else
    val hubId = s"${PeerJsSignaling.namespace}-room-${PeerJsSignaling.sanitize(room)}"

    var signaling: Option[PeerJsSignaling] = None
    var left                               = false
    // as non-hub: the connection to the hub, and when it was started
    var toHub: Option[(WebRTCHandling, Double)] = None
    var answering: Map[String, WebRTCHandling]  = Map.empty

    def newHandling(onReady: SessionDescription => Unit): WebRTCHandling = {
      val handling = WebRTCHandling(Some {
        case Success(sd) => onReady(sd)
        case Failure(ex) => errorReporter.fail(ex)
      })
      renderedConnectionTable.appendChild(handling.controlRow().render)
      addDataChannel(handling)
      handling
    }

    def restart(): Unit = {
      signaling.foreach(_.close())
      signaling = None
      toHub = None
      if !left then dom.window.setTimeout(() => if !left then asHub(), 1000 + Random.nextInt(2000)): Unit
    }

    def asHub(): Unit = {
      status(s"connecting to signaling server …")
      lazy val sig: PeerJsSignaling = PeerJsSignaling(
        hubId,
        new PeerJsSignaling.Handler {
          override def open(): Unit    = status(s"room “$room”: waiting for peers (hub)")
          override def idTaken(): Unit = if !left then asClient()
          override def offer(from: String, sd: SessionDescription): Unit = {
            val handling = newHandling(answer => sig.sendAnswer(from, answer))
            answering = answering.updated(from, handling)
            handling.peer.updateRemoteDescription(sd).run(errorReporter)
            status(s"room “$room”: hub, ${answering.size} peer(s) joined")
          }
          override def disconnected(): Unit = restart()
        }
      )
      signaling = Some(sig)
    }

    def asClient(): Unit = {
      lazy val sig: PeerJsSignaling = PeerJsSignaling(
        PeerJsSignaling.randomId(),
        new PeerJsSignaling.Handler {
          override def open(): Unit = {
            status(s"room “$room”: connecting to hub …")
            val handling = newHandling(offer => sig.sendOffer(hubId, offer))
            toHub = Some((handling, Date.now()))
            handling.peer.smartUpdateLocalDescription.run(errorReporter)
          }
          override def answer(from: String, sd: SessionDescription): Unit =
            toHub.foreach((handling, _) => handling.peer.updateRemoteDescription(sd).run(errorReporter))
          override def gone(peer: String): Unit = if peer == hubId then restart()
          override def disconnected(): Unit     = if toHub.isEmpty then restart()
        }
      )
      signaling = Some(sig)
    }

    // watch the connection to the hub, and try to take over if it fails
    val watchdog = dom.window.setInterval(
      () =>
        toHub.foreach { (handling, started) =>
          handling.peer.peerConnection.iceConnectionState match
              case RTCIceConnectionState.connected | RTCIceConnectionState.completed =>
                status(s"room “$room”: connected to hub")
              case RTCIceConnectionState.failed | RTCIceConnectionState.disconnected |
                  RTCIceConnectionState.closed =>
                restart()
              case _ if Date.now() - started > 30000 => restart()
              case _                                  => ()
        },
      5000
    )

    asHub()

    () => {
      left = true
      dom.window.clearInterval(watchdog)
      signaling.foreach(_.close())
      status("not connected to any room")
    }
  }

  def addDataChannel(handling: WebRTCHandling): Unit = {

    val channel = handling.peer.peerConnection.createDataChannel(
      channelLabel,
      new dom.RTCDataChannelInit {
        negotiated = true
        id = channelId
      }
    )

    dataManager.addClientConnection(WebRTCConnection.openLatent(channel))

  }

}

def errorReporter: Callback[Any] =
    case Success(_)  =>
    case Failure(ex) => throw ex

class WebRTCHandling(readyChannel: Option[Callback[SessionDescription]]) {

  val codec: JsonValueCodec[SessionDescription] = JsonCodecMaker.make

  val peer: WebRTCConnector = WebRTCConnector(new dom.RTCConfiguration {
    iceServers = js.Array[dom.RTCIceServer](new RTCIceServer {
      urls = js.Array[String]("stun:stun.t-online.de:3478", "stun:stun.l.google.com:19302")
    }, new RTCIceServer {
      // public TURN servers of PeerJS (credentials are published in the PeerJS client)
      urls = js.Array[String]("turn:eu-0.turn.peerjs.com:3478", "turn:us-0.turn.peerjs.com:3478")
      username = "peerjs"
      credential = "peerjsp"
    })
  })

  var readyChannelSent = false

  /** Pass the local session description on once ICE gathering is complete (or `force`d, e.g., after a timeout). */
  def signalReady(force: Boolean): Unit = {
    val overview = peer.readOverview
    if !readyChannelSent && (force || overview.iceGatheringState == RTCIceGatheringState.complete) then
        overview.localSession.foreach { sd =>
          readyChannelSent = true
          readyChannel.foreach(_.succeed(sd))
        }
  }

  // the gathering state may become complete without any of the events observed by `lifecycle`
  peer.peerConnection.addEventListener("icegatheringstatechange", (_: dom.Event) => signalReady(force = false))
  // gathering may take very long (or never finish) with unreachable STUN/TURN servers, use what we have by then
  if readyChannel.isDefined then dom.window.setTimeout(() => signalReady(force = true), 5000): Unit

  def controlRow(): ConcreteHtmlTag[dom.html.TableRow] = {

    val answerArea =
      textarea(
        placeholder := "remote session description",
        oninput     := { (ev: UIEvent) =>
          try
              val cs = readFromString(ev.target.asInstanceOf[dom.html.TextArea].value)(using codec)
              peer.updateRemoteDescription(cs).run(errorReporter)
          catch
              case ex: JsonReaderException =>
                throw IllegalStateException("input is not a valid session description", ex)
        }
      ).render

    val localSession    = td().render
    val remoteSession   = td().render
    val gatheringState  = td().render
    val connectionState = td().render
    val signalingState  = td().render

    val iceTrickle = textarea(readonly := true).render

    def sessionDisplay(sessionDescription: SessionDescription): dom.html.TextArea =
      textarea(
        readonly := true,
        onfocus  := { (ev: UIEvent) =>
          ev.target.asInstanceOf[dom.html.TextArea].select()
        },
        writeToString(sessionDescription)(using codec)
      ).render

    Async[Any] {
      val lifecycle: ConnectorOverview = peer.lifecycle.bind
      lifecycle.localSession match
          case Some(s) => localSession.replaceChildren(sessionDisplay(s))
          case None    => localSession.replaceChildren(span("no local session info").render)

      lifecycle.remoteSession match
          case Some(s) => remoteSession.replaceChildren(sessionDisplay(s))
          case None    => remoteSession.replaceChildren(answerArea)

      gatheringState.innerText = lifecycle.iceGatheringState
      connectionState.innerText = lifecycle.iceConnectionState
      signalingState.innerText = lifecycle.signalingState

      signalReady(force = false)

    }.run(errorReporter)

    Async {
      val dateOffset = Date.now()
      val candidate  = peer.iceCandidates.bind
      iceTrickle.value = s"${iceTrickle.value}${Date.now() - dateOffset}: ${candidate.candidate}\n\n"
    }.run(errorReporter)

    tr(localSession, remoteSession, gatheringState, connectionState, signalingState, td(iceTrickle))

  }

}
