package channels

import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, ConnectionDescriptor, TCP}

import java.io.{DataInputStream, InputStream}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Executors, LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Promise}
import scala.util.Try

/** Websocket clients connecting to the plain [[TCP]] listener. */
class TcpWebsocketTest extends munit.FunSuite {

  val handshake: Array[Byte] =
    ("GET / HTTP/1.1\r\nHost: example\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" +
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n").getBytes(
      StandardCharsets.US_ASCII
    )

  /** A masked client frame with a short payload. */
  def frame(opcode: Int, payload: Array[Byte]): Array[Byte] = {
    val mask = Array[Byte](1, 2, 3, 4)
    Array((0x80 | opcode).toByte, (0x80 | payload.length).toByte) ++ mask ++
    payload.indices.map(i => (payload(i) ^ mask(i & 3)).toByte)
  }

  def readHandshakeResponse(in: InputStream): String = {
    val sb = StringBuilder()
    while !sb.endsWith("\r\n\r\n") do {
      val b = in.read()
      assert(b >= 0, "eof during handshake response")
      sb.append(b.toChar)
    }
    sb.result()
  }

  /** Reads an unmasked server frame with a short payload: (opcode, payload). */
  def readFrame(in: DataInputStream): (Int, Seq[Byte]) = {
    val b0 = in.readUnsignedByte()
    val b1 = in.readUnsignedByte()
    (b0 & 0x0f, in.readNBytes(b1 & 0x7f).toSeq)
  }

  test("websocket handshake, messages in both directions, ping, and close") {
    val ec          = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    val abort       = Abort()
    val received    = LinkedBlockingQueue[Try[Seq[Byte]]]()
    val connections = LinkedBlockingQueue[Connection]()
    val ready       = Promise[ConnectionDescriptor]()

    TCP.listen(TCP.defaultServerSocket(InetSocketAddress(InetAddress.getLoopbackAddress, 0)), ec).listen { conn =>
      connections.put(conn)
      res => received.put(res.map(_.convertToArray().toSeq))
    }.runIn(abort)(ready.complete)

    val port = Await.result(ready.future, 5.seconds) match
        case ConnectionDescriptor.TcpWebSocket(_, port) => port
        case other                                      => fail(s"unexpected descriptor $other")

    val socket = Socket(InetAddress.getLoopbackAddress, port)
    socket.setSoTimeout(10000)
    val out = socket.getOutputStream
    val in  = DataInputStream(socket.getInputStream)
    try {
      out.write(handshake)
      val response = readHandshakeResponse(in)
      assert(response.contains("Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n"), response)

      out.write(frame(0x2, Array(1, 2, 3)))
      assertEquals(received.poll(10, TimeUnit.SECONDS).get, Seq[Byte](1, 2, 3))

      val conn = connections.poll(10, TimeUnit.SECONDS)
      conn.send(ByteBufferMessageBuffer(Array[Byte](4, 5))).run(_ => ())
      assertEquals(readFrame(in), (0x2, Seq[Byte](4, 5)))

      out.write(frame(0x9, Array(9)))
      assertEquals(readFrame(in), (0xa, Seq[Byte](9)))

      out.write(frame(0x8, Array(0x03, 0xe8.toByte)))
      assertEquals(readFrame(in)._1, 0x8)
      assert(received.poll(10, TimeUnit.SECONDS).isFailure)
      assertEquals(in.read(), -1)
    } finally {
      abort.closeRequest = true
      socket.close()
      ec.shutdown()
    }
  }
}
