package channels.connection

import channels.connection.WebsocketMultiplexing.{Framing, Output, State}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

class WebsocketMultiplexingTest extends munit.FunSuite {

  /** Pushes `data` into the protocol, in chunks of `chunk` bytes. */
  def run(data: Array[Byte], chunk: Int = Int.MaxValue): (State, List[Output]) = {
    val buffer = ReceiveBuffer()
    data.grouped(math.min(chunk, math.max(data.length, 1))).foldLeft((State.Init, List.empty[Output])) {
      case ((state, outputs), part) =>
        buffer.reserve(buffer.available + part.length)
        buffer.fill { bb =>
          bb.put(part); part.length
        }: Unit
        val (next, more) = WebsocketMultiplexing.receive(state, buffer)
        (next, outputs ++ more)
    }
  }

  def plain(payload: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(4 + payload.length).putInt(payload.length).put(payload).array()

  def messages(outputs: List[Output]): List[Seq[Byte]] = outputs.collect { case Output.Message(b) => b.toSeq }

  val handshake: Array[Byte] =
    "GET / HTTP/1.1\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n".getBytes(StandardCharsets.US_ASCII)

  /** Unmasked frame with a short payload. */
  def frame(opcode: Int, payload: Array[Byte], fin: Boolean = true): Array[Byte] =
    Array(((if fin then 0x80 else 0) | opcode).toByte, payload.length.toByte) ++ payload

  test("plain messages, detected once, in any chunking") {
    val data = plain(Array(1, 2, 3)) ++ plain(Array()) ++ plain(Array.fill(2000)(7))
    for chunk <- List(1, 3, 100, Int.MaxValue) do {
      val (state, outputs) = run(data, chunk)
      assertEquals(state, State.Plain)
      assertEquals(outputs.head, Output.Detected(Framing.Plain))
      assertEquals(messages(outputs), List(Seq[Byte](1, 2, 3), Seq(), Seq.fill[Byte](2000)(7)))
    }
  }

  test("websocket handshake, fragments, ping, and close") {
    val data = handshake ++ frame(0x2, Array(1), fin = false) ++ frame(0x9, Array(9)) ++ frame(0x0, Array(2)) ++
      frame(0x8, Array())
    for chunk <- List(1, 5, Int.MaxValue) do {
      val (state, outputs) = run(data ++ plain(Array(42)), chunk)
      assertEquals(state, State.Closed)
      outputs match
          case Output.Reply(response) :: Output.Detected(Framing.WebSocket) :: Output.Reply(pong) ::
              Output.Message(message) :: Output.Reply(close) :: Output.Closed :: Nil =>
            assert(String(response, StandardCharsets.US_ASCII).startsWith("HTTP/1.1 101"))
            assertEquals(pong.toSeq, frame(0xa, Array(9)).toSeq)
            assertEquals(message.toSeq, Seq[Byte](1, 2))
            assertEquals(close.toSeq, WebsocketProtocol.encodeCloseFrame().toSeq)
          case other => fail(s"unexpected outputs: $other")
    }
  }

  /** One side of a connection: its state and receive buffer. */
  class Side(var state: State) {
    val buffer = ReceiveBuffer()

    /** Receives `bytes`, returns the outputs. */
    def receive(bytes: Array[Byte]): List[Output] = {
      buffer.reserve(buffer.available + bytes.length)
      buffer.fill { bb =>
        bb.put(bytes); bytes.length
      }: Unit
      val (next, outputs) = WebsocketMultiplexing.receive(state, buffer)
      state = next
      outputs
    }
  }

  def send(framing: Framing, payload: Array[Byte]): Array[Byte] =
    WebsocketMultiplexing.frame(framing, ByteBuffer.wrap(payload)).flatMap(MessageBuffer.convertByteBufferToArray)

  /** Whether `frame` is marked as going from the client to the server (the mask bit). */
  def fromClient(frame: Array[Byte]): Boolean = (frame(1) & 0x80) != 0

  test("websocket client and server") {
    val (initial, request) = WebsocketMultiplexing.client("example", "/path")
    val client             = Side(initial)
    val server             = Side(State.Init)

    val (response, serverFrame) = server.receive(request) match
        case List(Output.Reply(response), Output.Detected(Framing.WebSocket)) =>
          (response, send(Framing.WebSocket, Array(1, 2)))
        case other => fail(s"unexpected server outputs: $other")
    assert(!fromClient(serverFrame))

    // response and first message arrive together
    client.receive(response ++ serverFrame) match
        case List(Output.Detected(Framing.WebSocketClient), Output.Message(m)) => assertEquals(m.toSeq, Seq[Byte](1, 2))
        case other => fail(s"unexpected client outputs: $other")

    val clientFrame = send(Framing.WebSocketClient, Array.tabulate[Byte](300)(_.toByte))
    assert(fromClient(clientFrame))
    assertEquals(messages(server.receive(clientFrame)), List(Seq.tabulate[Byte](300)(_.toByte)))

    // the client answers a ping with a pong from the client
    client.receive(WebsocketProtocol.encodeFrame(0x9, ByteBuffer.wrap(Array[Byte](7)), fromClient = false)) match
        case List(Output.Reply(pong)) =>
          assert(fromClient(pong))
          assertEquals(server.receive(pong), Nil)
        case other => fail(s"unexpected client outputs: $other")

    // closing from the client side
    val goodbye = WebsocketMultiplexing.goodbye(Framing.WebSocketClient).get
    assert(fromClient(goodbye))
    server.receive(goodbye) match
        case List(Output.Reply(close), Output.Closed) =>
          assertEquals(client.receive(close).last, Output.Closed)
        case other => fail(s"unexpected server outputs: $other")
    assertEquals(server.state, State.Closed)
    assertEquals(client.state, State.Closed)
  }

  test("websocket client rejects a wrong accept") {
    val (initial, _) = WebsocketMultiplexing.client("example", "/")
    val response     =
      "HTTP/1.1 101 Switching Protocols\r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\r\n"
    intercept[IllegalStateException](Side(initial).receive(response.getBytes(StandardCharsets.US_ASCII)))
  }

  test("websocket client rejects a refused handshake") {
    val (initial, _) = WebsocketMultiplexing.client("example", "/")
    intercept[IllegalStateException](
      Side(initial).receive("HTTP/1.1 404 Not Found\r\n\r\n".getBytes(StandardCharsets.US_ASCII))
    )
  }

  test("too large messages are rejected") {
    intercept[IllegalArgumentException](run(ByteBuffer.allocate(4).putInt(MessageBuffer.maxPayloadSize).array()))
  }
}
