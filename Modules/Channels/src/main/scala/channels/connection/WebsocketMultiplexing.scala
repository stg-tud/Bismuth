package channels
package connection

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

/** Messages on a byte stream, either length-prefixed ("plain"), or as a websocket (handshake, then frames).
  * The remote side decides: a stream starting with a websocket handshake is a websocket, anything else is plain.
  *
  * A state machine, independent of where the bytes come from: [[receive]] parses what is buffered, and returns what
  * to do about it as [[WebsocketMultiplexing.Output]]s.
  */
object WebsocketMultiplexing {

  /** How messages are framed on the byte stream. */
  enum Framing {
    case Plain, WebSocket
  }

  enum State {

    /** Nothing received yet, the protocol is not known. */
    case Init
    case Plain
    case WebSocketHandshake

    /** `fragments` of a message whose last frame did not arrive yet. */
    case WebSocket(fragments: Vector[Array[Byte]])

    /** The remote side closed the stream, nothing more is parsed. */
    case Closed
  }

  enum Output {

    /** The protocol of the remote side is known, send using `framing`. */
    case Detected(framing: Framing)

    /** A complete message. */
    case Message(bytes: Array[Byte])

    /** Bytes to send back, as part of the protocol (handshake response, pong, close). */
    case Reply(bytes: Array[Byte])

    /** The remote side closed the stream: close it once the replies are written. */
    case Closed
  }

  /** Parses all complete units (messages, frames, handshake) from `buffer`, consuming them. Leaves incomplete ones. */
  def receive(state: State, buffer: ReceiveBuffer): (State, List[Output]) = {
    val outputs = List.newBuilder[Output]
    var current = state
    var done    = false
    while !done do
        step(current, buffer) match
            case None                  => done = true
            case Some((next, results)) =>
              current = next
              outputs ++= results
    (current, outputs.result())
  }

  /** The header to send before a message of `length` bytes. */
  def header(framing: Framing, length: Int): ByteBuffer = framing match
      case Framing.Plain     => ByteBuffer.allocate(4).putInt(0, length)
      case Framing.WebSocket => ByteBuffer.wrap(WebsocketProtocol.frameHeader(0x2, length))

  /** What to send before closing the stream. */
  def goodbye(framing: Framing): Option[Array[Byte]] = framing match
      case Framing.Plain     => None
      case Framing.WebSocket => Some(WebsocketProtocol.encodeCloseFrame())

  /** Parses the next complete unit, if there is one. */
  private def step(state: State, buffer: ReceiveBuffer): Option[(State, List[Output])] = state match {
    case State.Init =>
      if buffer.available < 4 then None
      else if buffer.peekInt() == WebsocketProtocol.handshakePrefix then Some((State.WebSocketHandshake, Nil))
      else Some((State.Plain, List(Output.Detected(Framing.Plain))))

    case State.Plain =>
      if buffer.available < 4 then None
      else {
        val len = buffer.peekInt()
        require(len < MessageBuffer.maxPayloadSize, "message too large")
        if buffer.available < 4 + len then
            buffer.reserve(4 + len)
            None
        else
            buffer.consume(4)
            Some((State.Plain, List(Output.Message(buffer.take(len)))))
      }

    case State.WebSocketHandshake =>
      val view = buffer.view
      WebsocketProtocol.tryParseHandshake(view).map { request =>
        buffer.consume(view.position())
        (
          State.WebSocket(Vector.empty),
          List(Output.Reply(WebsocketProtocol.handshakeResponse(request)), Output.Detected(Framing.WebSocket))
        )
      }

    case State.WebSocket(fragments) =>
      val view = buffer.view
      WebsocketProtocol.tryParseHeader(view).flatMap { header =>
        require(header.len < MessageBuffer.maxPayloadSize, "message too large")
        val headerLength = view.position()
        if buffer.available < headerLength + header.len then
            buffer.reserve(headerLength + header.len)
            None
        else
            buffer.consume(headerLength)
            val frame = WebsocketProtocol.tryParsePayload(header, buffer.take(header.len))
            Some(websocketFrame(header.fin, frame, fragments))
      }

    case State.Closed => None
  }

  private def websocketFrame(
      fin: Boolean,
      frame: WebsocketFrame,
      fragments: Vector[Array[Byte]]
  ): (State, List[Output]) = {

    def data(bytes: Array[Byte]): (State, List[Output]) =
      if fin then
          val message = if fragments.isEmpty then bytes else (fragments :+ bytes).toArray.flatten
          (State.WebSocket(Vector.empty), List(Output.Message(message)))
      else (State.WebSocket(fragments :+ bytes), Nil)

    frame match {
      case BinaryFrame(bytes)        => data(bytes)
      case TextFrame(text)           => data(text.getBytes(StandardCharsets.UTF_8))
      case ReservedFrame(0x0, bytes) => data(bytes)
      case PingFrame(bytes)          =>
        (State.WebSocket(fragments), List(Output.Reply(WebsocketProtocol.encodePongFrame(bytes))))
      case PongFrame(_) | ReservedFrame(_, _) => (State.WebSocket(fragments), Nil)
      case CloseFrame(_, _)                   =>
        (State.Closed, List(Output.Reply(WebsocketProtocol.encodeCloseFrame()), Output.Closed))
    }
  }
}
