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

  test("too large messages are rejected") {
    intercept[IllegalArgumentException](run(ByteBuffer.allocate(4).putInt(MessageBuffer.maxPayloadSize).array()))
  }
}
