package webapps

import channels.webrtc.SessionDescription
import org.scalajs.dom

import scala.scalajs.js
import scala.util.Random

/** Minimal client for the signaling protocol of a PeerJS server (https://github.com/peers/peerjs-server),
  * such as the public one at `0.peerjs.com`. Does not use any PeerJS code, only the wire protocol:
  *
  * - connect via websocket to `wss://host:port/path/peerjs?key=…&id=…&token=…`, the server replies `OPEN`,
  *   or `ID-TAKEN` if another client uses that id
  * - send `{type, payload, dst}`, the server forwards `OFFER`, `ANSWER`, `CANDIDATE`, … as `{type, payload, src}`
  *   to the client with id `dst`. The public server validates that the payload looks like one of the PeerJS client
  *   (`{sdp: {type, sdp}, type: "data", connectionId, …}` with a valid SDP) and drops the sender otherwise
  * - send a `HEARTBEAT` regularly, otherwise the server drops the connection
  *
  * Note that the public server does not allow to list peers, so peers have to agree on ids out of band.
  */
class PeerJsSignaling(
    val id: String,
    handler: PeerJsSignaling.Handler,
    host: String = PeerJsSignaling.cloudHost,
    port: Int = 443,
    path: String = "/",
    key: String = "peerjs",
    heartbeatMillis: Int = 5000,
) {
  import PeerJsSignaling.*

  require(validId(id), s"invalid peerjs id: $id")

  private val token = Random.alphanumeric.take(12).mkString.toLowerCase

  /** PeerJS identifies data connections by this id, we only have one data channel per peer connection */
  private val connectionId = s"dc_${Random.alphanumeric.take(12).mkString.toLowerCase}"

  private val socket = new dom.WebSocket(
    s"wss://$host:$port${path}peerjs?key=$key&id=$id&token=$token&version=1.5.5"
  )

  @volatile private var closed = false

  private val heartbeat: Int = dom.window.setInterval(
    () => if socket.readyState == dom.WebSocket.OPEN then socket.send(js.JSON.stringify(js.Dynamic.literal(`type` = "HEARTBEAT"))),
    heartbeatMillis
  )

  socket.onmessage = (event: dom.MessageEvent) => {
    val msg     = js.JSON.parse(event.data.asInstanceOf[String])
    if msg.`type`.asInstanceOf[String] != "HEARTBEAT" then
        println(s"peerjs[$id] received ${msg.`type`} from ${msg.src}")
    val payload = msg.payload
    def payloadDescription: Option[SessionDescription] =
      if js.isUndefined(payload) || payload == null || js.isUndefined(payload.sdp) || payload.sdp == null then None
      else Some(SessionDescription(payload.sdp.`type`.asInstanceOf[String], payload.sdp.sdp.asInstanceOf[String]))
    val src = msg.src.asInstanceOf[js.UndefOr[String]].getOrElse("")
    msg.`type`.asInstanceOf[String] match
        case "OPEN"                                 => handler.open()
        case "ID-TAKEN"                             => shutdown(); handler.idTaken()
        case "OFFER"                                => payloadDescription.foreach(handler.offer(src, _))
        case "ANSWER"                               => payloadDescription.foreach(handler.answer(src, _))
        case "LEAVE" | "EXPIRE"                     => handler.gone(src)
        case "ERROR" | "INVALID-KEY"                => handler.error(js.JSON.stringify(msg))
        case _                                      => ()
  }

  socket.onclose = (event: dom.CloseEvent) =>
    if !closed then {
      println(s"peerjs[$id] connection closed by server (code ${event.code})")
      shutdown()
      handler.disconnected()
    }
  socket.onerror = (_: dom.Event) => handler.error(s"websocket error ($host)")

  def isOpen: Boolean = !closed && socket.readyState == dom.WebSocket.OPEN

  def sendOffer(dst: String, sd: SessionDescription): Unit  = send("OFFER", dst, sd)
  def sendAnswer(dst: String, sd: SessionDescription): Unit = send("ANSWER", dst, sd)

  private def send(tpe: String, dst: String, sd: SessionDescription): Unit =
    println(s"peerjs[$id] sending $tpe to $dst (socket open: $isOpen)")
    if isOpen then
        socket.send(js.JSON.stringify(js.Dynamic.literal(
          `type` = tpe,
          dst = dst,
          // the public server only forwards payloads that look like those of the PeerJS client (with a valid SDP),
          // and closes the connection of the sender otherwise
          payload = js.Dynamic.literal(
            sdp = js.Dynamic.literal(`type` = sd.descType, sdp = sd.sdp),
            `type` = "data",
            connectionId = connectionId,
            label = connectionId,
            reliable = true,
            serialization = "raw",
          )
        )))

  private def shutdown(): Unit = {
    closed = true
    dom.window.clearInterval(heartbeat)
  }

  def close(): Unit = {
    shutdown()
    socket.close()
  }
}

object PeerJsSignaling {
  val cloudHost = "0.peerjs.com"

  trait Handler {
    def open(): Unit                                         = ()
    def idTaken(): Unit                                      = ()
    def offer(from: String, sd: SessionDescription): Unit    = ()
    def answer(from: String, sd: SessionDescription): Unit   = ()
    def gone(peer: String): Unit                             = ()
    def disconnected(): Unit                                 = ()
    def error(message: String): Unit                         = println(s"peerjs signaling error: $message")
  }

  def validId(id: String): Boolean = id.matches("^[A-Za-z0-9]+(?:[ _-][A-Za-z0-9]+)*$")

  /** Prefix for all ids we use on the (shared, public) server. */
  val namespace = "stg-tud-bismuth-q7x3k9w2"

  def randomId(): String = s"$namespace-peer-${Random.alphanumeric.take(16).mkString}"

  /** Turn arbitrary text into a valid id. */
  def sanitize(name: String): String =
    name.split("[^A-Za-z0-9]+").filter(_.nonEmpty).mkString("-")
}
