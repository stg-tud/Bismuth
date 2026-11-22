package channels.connection

import de.rmgk.delay.Async

import java.io.{DataInputStream, InputStream, OutputStream}
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

/** Checks the bytes on the wire of [[NioTCP]] (plain length-prefixed framing and websockets), against raw sockets,
  * including messages split across or combined within reads.
  */
class NioTCPWireTest extends munit.FunSuite {

  class Server {
    val nio: NioTCP                                     = NioTCP()
    val abort: Abort                                    = Abort()
    val received: LinkedBlockingQueue[Try[Array[Byte]]] = LinkedBlockingQueue()
    val connections: LinkedBlockingQueue[Connection]    = LinkedBlockingQueue()
    private val ready                                   = Promise[ConnectionDescriptor]()

    nio.listen(nio.defaultServerSocketChannel(InetSocketAddress("127.0.0.1", 0))).listen { conn =>
      connections.put(conn)
      res => received.put(res.map(_.convertToArray()))
    }.runIn(Abort())(ready.complete)

    val port: Int = Await.result(ready.future, 5.seconds) match
        case ConnectionDescriptor.TcpWebSocket(_, port) => port
        case other                                      => throw IllegalStateException(other.toString)

    private val thread = Thread(() => nio.loopSelection(abort))
    thread.setDaemon(true)
    thread.start()

    def next(): Try[Array[Byte]] = {
      val res = received.poll(10, TimeUnit.SECONDS)
      assert(res != null, "nothing received")
      res
    }
    def nextMessage(): Array[Byte] = next().get
    def connection(): Connection   = {
      val c = connections.poll(10, TimeUnit.SECONDS)
      assert(c != null, "no connection")
      c
    }
    def close(): Unit = {
      abort.closeRequest = true
      nio.selector.keys().forEach(_.channel().close())
    }
  }

  def withServer(body: (Server, Socket) => Unit): Unit = {
    val server = Server()
    val socket = Socket("127.0.0.1", server.port)
    socket.setTcpNoDelay(true)
    socket.setSoTimeout(10000)
    try body(server, socket)
    finally {
      socket.close()
      server.close()
    }
  }

  def bytes(n: Int, seed: Int = 0): Array[Byte] = Array.tabulate(n)(i => ((i * 31 + seed) & 0xff).toByte)

  def plainFrame(payload: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(4 + payload.length).putInt(payload.length).put(payload).array()

  /** Writes in chunks of `chunk` bytes, pausing so that the server sees them in separate reads. */
  def writeSlowly(out: OutputStream, data: Array[Byte], chunk: Int): Unit =
    data.grouped(chunk).foreach { c =>
      out.write(c)
      out.flush()
      Thread.sleep(2)
    }

  def assertBytes(obtained: Array[Byte], expected: Array[Byte])(using munit.Location): Unit =
    assertEquals(obtained.toSeq, expected.toSeq)

  def await[T](async: Async[Abort, T]): T = {
    val p = Promise[T]()
    async.runIn(Abort())(p.complete)
    Await.result(p.future, 10.seconds)
  }

  val sizes: List[Int] = List(0, 1, 4, 1000, 1020, 1024, 1025, 3000, 100_000)

  // ------------------------------------------------------------------------------------------------
  // plain
  // ------------------------------------------------------------------------------------------------

  test("plain: many messages in a single write") {
    withServer { (server, socket) =>
      val payloads = sizes.zipWithIndex.map((s, i) => bytes(s, i))
      socket.getOutputStream.write(payloads.flatMap(plainFrame).toArray)
      payloads.foreach(p => assertBytes(server.nextMessage(), p))
    }
  }

  test("plain: messages split into small chunks") {
    withServer { (server, socket) =>
      val payloads = List(0, 1, 5, 1023, 1024, 1025, 2500).zipWithIndex.map((s, i) => bytes(s, i))
      writeSlowly(socket.getOutputStream, payloads.flatMap(plainFrame).toArray, 3)
      payloads.foreach(p => assertBytes(server.nextMessage(), p))
    }
  }

  test("plain: chunk boundaries at every offset of a frame") {
    withServer { (server, socket) =>
      val out   = socket.getOutputStream
      val frame = plainFrame(bytes(10))
      (1 until frame.length).foreach { split =>
        out.write(frame, 0, split)
        out.flush()
        Thread.sleep(5)
        out.write(frame, split, frame.length - split)
        out.flush()
        assertBytes(server.nextMessage(), bytes(10))
      }
    }
  }

  test("plain: server sends length-prefixed messages") {
    withServer { (server, socket) =>
      socket.getOutputStream.write(plainFrame(bytes(3)))
      server.nextMessage()
      val conn = server.connection()
      val in   = DataInputStream(socket.getInputStream)
      sizes.foreach { s =>
        await(conn.send(ByteBufferMessageBuffer(bytes(s, 7))))
        assertEquals(in.readInt(), s)
        assertBytes(in.readNBytes(s), bytes(s, 7))
      }
    }
  }

  test("plain: end of stream fails the message callback") {
    withServer { (server, socket) =>
      socket.getOutputStream.write(plainFrame(bytes(3)))
      server.nextMessage()
      socket.shutdownOutput()
      assert(server.next().failed.get.isInstanceOf[NoMoreDataException])
    }
  }

  test("plain: outgoing connection sends and receives") {
    val raw   = ServerSocket(0, 1, java.net.InetAddress.getByName("127.0.0.1"))
    val nio   = NioTCP()
    val abort = Abort()
    val t     = Thread(() => nio.loopSelection(abort))
    t.setDaemon(true)
    t.start()
    val received = LinkedBlockingQueue[Array[Byte]]()
    try {
      val conn = await(nio.connect(nio.defaultSocketChannel(InetSocketAddress("127.0.0.1", raw.getLocalPort)))
        .connect(_ => res => res.foreach(m => received.put(m.convertToArray()))))
      val peer = raw.accept()
      peer.setSoTimeout(10000)
      val in = DataInputStream(peer.getInputStream)
      await(conn.send(ByteBufferMessageBuffer(bytes(2000))))
      assertEquals(in.readInt(), 2000)
      assertBytes(in.readNBytes(2000), bytes(2000))
      val payloads = sizes.map(bytes(_, 3))
      peer.getOutputStream.write(payloads.flatMap(plainFrame).toArray)
      payloads.foreach(p => assertBytes(received.poll(10, TimeUnit.SECONDS), p))
      peer.close()
    } finally {
      abort.closeRequest = true
      nio.selector.keys().forEach(_.channel().close())
      raw.close()
    }
  }

  // ------------------------------------------------------------------------------------------------
  // websocket
  // ------------------------------------------------------------------------------------------------

  val handshake: Array[Byte] =
    ("GET /chat HTTP/1.1\r\nHost: example\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" +
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n").getBytes(
      StandardCharsets.US_ASCII
    )

  /** A client frame, masked as clients must. */
  def wsFrame(
      opcode: Int,
      payload: Array[Byte],
      fin: Boolean = true,
      mask: Array[Byte] | Null = Array(1, 2, 3, 4)
  ): Array[Byte] = {
    val buf = ByteBuffer.allocate(14 + payload.length)
    buf.put(((if fin then 0x80 else 0) | opcode).toByte)
    val maskBit = if mask == null then 0 else 0x80
    if payload.length < 126 then buf.put((maskBit | payload.length).toByte)
    else if payload.length <= 0xffff then buf.put((maskBit | 126).toByte).putShort(payload.length.toShort)
    else buf.put((maskBit | 127).toByte).putLong(payload.length.toLong)
    if mask != null then buf.put(mask): Unit
    payload.indices.foreach(i => buf.put((payload(i) ^ (if mask == null then 0 else mask(i & 3).toInt)).toByte))
    java.util.Arrays.copyOf(buf.array(), buf.position())
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

  /** Reads an (unmasked) server frame: (fin, opcode, payload). */
  def readFrame(in: DataInputStream): (Boolean, Int, Array[Byte]) = {
    val b0 = in.readUnsignedByte()
    val b1 = in.readUnsignedByte()
    assertEquals(b1 & 0x80, 0, "server frames are not masked")
    val len = (b1 & 0x7f) match
        case 126 => in.readUnsignedShort().toLong
        case 127 => in.readLong()
        case l   => l.toLong
    ((b0 & 0x80) != 0, b0 & 0x0f, in.readNBytes(len.toInt))
  }

  def checkHandshakeResponse(response: String): Unit = {
    assert(response.startsWith("HTTP/1.1 101 Switching Protocols\r\n"), response)
    assert(response.contains("Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n"), response)
  }

  test("websocket: handshake and frames in a single write") {
    withServer { (server, socket) =>
      val payloads = sizes.zipWithIndex.map((s, i) => bytes(s, i))
      socket.getOutputStream.write(handshake ++ payloads.flatMap(wsFrame(0x2, _)))
      checkHandshakeResponse(readHandshakeResponse(socket.getInputStream))
      payloads.foreach(p => assertBytes(server.nextMessage(), p))
    }
  }

  test("websocket: handshake and frames split into small chunks") {
    withServer { (server, socket) =>
      val payloads = List(0, 1, 125, 126, 1020, 1025, 2500).zipWithIndex.map((s, i) => bytes(s, i))
      writeSlowly(socket.getOutputStream, handshake ++ payloads.flatMap(wsFrame(0x2, _)), 3)
      checkHandshakeResponse(readHandshakeResponse(socket.getInputStream))
      payloads.foreach(p => assertBytes(server.nextMessage(), p))
    }
  }

  test("websocket: unmasked, text, and fragmented frames") {
    withServer { (server, socket) =>
      val out = socket.getOutputStream
      out.write(handshake)
      checkHandshakeResponse(readHandshakeResponse(socket.getInputStream))
      out.write(wsFrame(0x2, bytes(10), mask = null))
      assertBytes(server.nextMessage(), bytes(10))
      out.write(wsFrame(0x1, "hällo".getBytes(StandardCharsets.UTF_8)))
      assertBytes(server.nextMessage(), "hällo".getBytes(StandardCharsets.UTF_8))
      out.write(
        wsFrame(0x2, bytes(5, 1), fin = false) ++ wsFrame(0x0, bytes(2000, 2), fin = false) ++
        wsFrame(0x0, bytes(7, 3))
      )
      assertBytes(server.nextMessage(), bytes(5, 1) ++ bytes(2000, 2) ++ bytes(7, 3))
      out.write(wsFrame(0x2, bytes(4, 9)))
      assertBytes(server.nextMessage(), bytes(4, 9))
    }
  }

  test("websocket: ping is answered with pong, also within a fragmented message") {
    withServer { (server, socket) =>
      val out = socket.getOutputStream
      val in  = DataInputStream(socket.getInputStream)
      out.write(handshake)
      checkHandshakeResponse(readHandshakeResponse(in))
      out.write(wsFrame(0x9, bytes(6)))
      assertEquals(readFrame(in) match { case (f, o, p) => (f, o, p.toSeq) }, (true, 0xa, bytes(6).toSeq))
      out.write(wsFrame(0x2, bytes(3, 1), fin = false) ++ wsFrame(0x9, bytes(2)) ++ wsFrame(0x0, bytes(3, 2)))
      assertEquals(readFrame(in) match { case (f, o, p) => (f, o, p.toSeq) }, (true, 0xa, bytes(2).toSeq))
      assertBytes(server.nextMessage(), bytes(3, 1) ++ bytes(3, 2))
    }
  }

  test("websocket: server sends binary frames") {
    withServer { (server, socket) =>
      val out = socket.getOutputStream
      val in  = DataInputStream(socket.getInputStream)
      out.write(handshake)
      checkHandshakeResponse(readHandshakeResponse(in))
      val conn = server.connection()
      sizes.foreach { s =>
        await(conn.send(ByteBufferMessageBuffer(bytes(s, 5))))
        val (fin, opcode, payload) = readFrame(in)
        assert(fin)
        assertEquals(opcode, 0x2)
        assertBytes(payload, bytes(s, 5))
      }
    }
  }

  test("websocket: close frame is answered and fails the callback") {
    withServer { (server, socket) =>
      val out = socket.getOutputStream
      val in  = DataInputStream(socket.getInputStream)
      out.write(handshake)
      checkHandshakeResponse(readHandshakeResponse(in))
      out.write(wsFrame(0x8, Array[Byte](0x03, 0xe8.toByte)))
      val (fin, opcode, payload) = readFrame(in)
      assert(fin)
      assertEquals(opcode, 0x8)
      assertBytes(payload, Array[Byte](0x03, 0xe8.toByte))
      assert(server.next().failed.get.isInstanceOf[NoMoreDataException])
      assertEquals(in.read(), -1)
    }
  }

  test("websocket: closing the connection sends a close frame") {
    withServer { (server, socket) =>
      val in = DataInputStream(socket.getInputStream)
      socket.getOutputStream.write(handshake)
      checkHandshakeResponse(readHandshakeResponse(in))
      server.connection().close()
      val (_, opcode, payload) = readFrame(in)
      assertEquals(opcode, 0x8)
      assertBytes(payload, Array[Byte](0x03, 0xe8.toByte))
      assertEquals(in.read(), -1)
    }
  }
}
