package channels
package connection

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

/** Messages on a byte stream, either length-prefixed ("plain"), or as a websocket (handshake, then frames).
  * The remote side decides: a stream starting with a websocket handshake is a websocket, anything else is plain.
  * Alternatively, we are the websocket client, see [[client]].
  *
  * A state machine, independent of where the bytes come from: [[receive]] parses what is buffered, and returns what
  * to do about it as [[WebsocketMultiplexing.Output]]s.
  */
object WebsocketMultiplexing {

  /** How messages are framed on the byte stream. */
  enum Framing {
    case Plain

    /** Websocket frames, as the server. */
    case WebSocket

    /** Websocket frames, as the client (marked as going from the client to the server). */
    case WebSocketClient
  }

  enum State {

    /** Nothing received yet, the protocol is not known. */
    case Init
    case Plain
    case WebSocketHandshake

    /** We are the websocket client, waiting for the server to accept our handshake. */
    case ClientHandshake

    /** `fragments` of a message whose last frame did not arrive yet. `client` if we are the websocket client. */
    case WebSocket(fragments: Vector[Array[Byte]], client: Boolean = false)

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

  /** Starts as the websocket client of a server at `host`, for `path`: the initial state, and the handshake request
    * to send.
    */
  def client(host: String, path: String): (State, Array[Byte]) =
    (State.ClientHandshake, WebsocketProtocol.handshakeRequest(host, path))

  /** What to send for the message `payload` (the remaining bytes, which must not change afterwards). */
  def frame(framing: Framing, payload: ByteBuffer): Array[ByteBuffer] = framing match
      case Framing.Plain     => Array(ByteBuffer.allocate(4).putInt(0, payload.remaining()), payload)
      case Framing.WebSocket => Array(ByteBuffer.wrap(WebsocketProtocol.frameHeader(0x2, payload.remaining())), payload)
      case Framing.WebSocketClient =>
        Array(ByteBuffer.wrap(WebsocketProtocol.frameHeader(0x2, payload.remaining(), fromClient = true)), payload)

  /** What to send before closing the stream. */
  def goodbye(framing: Framing): Option[Array[Byte]] = framing match
      case Framing.Plain           => None
      case Framing.WebSocket       => Some(WebsocketProtocol.encodeCloseFrame())
      case Framing.WebSocketClient => Some(WebsocketProtocol.encodeCloseFrame(fromClient = true))

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

    case State.ClientHandshake =>
      val view = buffer.view
      Option.when(WebsocketProtocol.tryParseHandshakeResponse(view)) {
        buffer.consume(view.position())
        (State.WebSocket(Vector.empty, client = true), List(Output.Detected(Framing.WebSocketClient)))
      }

    case State.WebSocket(fragments, client) =>
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
            Some(websocketFrame(header.fin, frame, fragments, client))
      }

    case State.Closed => None
  }

  private def websocketFrame(
      fin: Boolean,
      frame: WebsocketFrame,
      fragments: Vector[Array[Byte]],
      client: Boolean,
  ): (State, List[Output]) = {

    def data(bytes: Array[Byte]): (State, List[Output]) =
      if fin then
          val message = if fragments.isEmpty then bytes else (fragments :+ bytes).toArray.flatten
          (State.WebSocket(Vector.empty, client), List(Output.Message(message)))
      else (State.WebSocket(fragments :+ bytes, client), Nil)

    frame match {
      case BinaryFrame(bytes)        => data(bytes)
      case TextFrame(text)           => data(text.getBytes(StandardCharsets.UTF_8))
      case ReservedFrame(0x0, bytes) => data(bytes)
      case PingFrame(bytes)          =>
        (
          State.WebSocket(fragments, client),
          List(Output.Reply(WebsocketProtocol.encodePongFrame(bytes, fromClient = client)))
        )
      case PongFrame(_) | ReservedFrame(_, _) => (State.WebSocket(fragments, client), Nil)
      case CloseFrame(_, _)                   =>
        (State.Closed, List(Output.Reply(WebsocketProtocol.encodeCloseFrame(fromClient = client)), Output.Closed))
    }
  }
}
