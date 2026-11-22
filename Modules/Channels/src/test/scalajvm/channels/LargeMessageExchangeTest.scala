package channels

import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, MessageBuffer, NioTCP}

import java.net.StandardProtocolFamily
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.ByteBuffer
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success}

/** Two peers that answer a message they receive with a large one, on the selector thread (as [[MessagingIO]] does).
  * If both answer at the same time with more than the socket buffers hold, each selector thread must keep reading
  * while its message is not sent yet, or the two wait for each other forever.
  */
class LargeMessageExchangeTest extends munit.FunSuite {

  lazy val isMacOS: Boolean         = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))
  override def munitIgnore: Boolean = isMacOS

  val largeSize: Int = 8 << 20

  def large(): MessageBuffer = ByteBufferMessageBuffer(Array.fill[Byte](largeSize)(7))

  def ping(): MessageBuffer = ByteBufferMessageBuffer("ping".getBytes())

  def loop(nio: NioTCP, abort: Abort, name: String): Unit = {
    val thread = Thread(() => nio.loopSelection(abort), name)
    thread.setDaemon(true)
    thread.start()
  }

  test("both peers sending a large message at once on their selector threads") {
    val socketPath = domainSocketHelperNonensese("large-message-exchange")
    val server     = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.configureBlocking(false)
    server.bind(socketPath)

    val serverNio = NioTCP()
    val clientNio = NioTCP()
    val abort     = Abort()
    loop(serverNio, abort, "server-selector")
    loop(clientNio, abort, "client-selector")

    val serverGotLarge = CountDownLatch(1)
    val clientGotLarge = CountDownLatch(1)

    def failOnError(ex: Throwable): Unit = { ex.printStackTrace(); () }

    // the server answers the ping of the client with a ping and a large message
    serverNio.listen(() => server).listen { conn =>
      {
        case Success(mb) if mb.asByteBuffer.remaining() == largeSize => serverGotLarge.countDown()
        case Success(_)                                              =>
          conn.send(ping()).run(_ => ())
          conn.send(large()).run(_.failed.foreach(failOnError))
        case Failure(_) => ()
      }
    }.runIn(Abort())(_.failed.foreach(failOnError))

    // the client answers the ping of the server with a large message
    clientNio.connect { () =>
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(socketPath)
      channel
    }.connect { conn =>
      {
        case Success(mb) if mb.asByteBuffer.remaining() == largeSize => clientGotLarge.countDown()
        case Success(_) => conn.send(large()).run(_.failed.foreach(failOnError))
        case Failure(_) => ()
      }
    }.runIn(Abort()) {
      case Success(conn) =>
        conn.send(ping()).run(_.failed.foreach(failOnError))
      case Failure(ex) => failOnError(ex)
    }

    try {
      assert(clientGotLarge.await(20, TimeUnit.SECONDS), "client did not get the large message")
      assert(serverGotLarge.await(20, TimeUnit.SECONDS), "server did not get the large message")
    } finally {
      abort.closeRequest = true
      // also frees selector threads stuck writing
      serverNio.selector.keys().forEach(_.channel().close())
      clientNio.selector.keys().forEach(_.channel().close())
    }
  }

  test("sending completes only once the peer took the message") {
    val socketPath = domainSocketHelperNonensese("large-message-backpressure")
    val server     = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(socketPath)

    val clientNio = NioTCP()
    val abort     = Abort()
    loop(clientNio, abort, "client-selector")

    val conn = Promise[Connection]()
    clientNio.connect { () =>
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(socketPath)
      channel
    }.connect(_ => _ => ()).runIn(Abort())(conn.complete)
    // a peer that does not read until told to
    val peer = server.accept()

    try {
      val connection = Await.result(conn.future, 10.seconds)

      val smallSent = CountDownLatch(1)
      connection.send(ping()).run(r => if r.isSuccess then smallSent.countDown())
      assert(smallSent.await(10, TimeUnit.SECONDS), "a small message fits into the socket buffers")

      val largeSent = CountDownLatch(1)
      connection.send(large()).run(r => if r.isSuccess then largeSent.countDown())
      assert(!largeSent.await(500, TimeUnit.MILLISECONDS), "large message reported sent while the peer does not read")

      // the peer reads everything: ping and the large message, each with its length
      val expected = (4 + 4) + (4 + largeSize)
      val buffer   = ByteBuffer.allocate(expected)
      while buffer.hasRemaining && peer.read(buffer) >= 0 do ()
      assertEquals(buffer.position(), expected)
      assert(largeSent.await(10, TimeUnit.SECONDS), "large message not reported sent once the peer read it")
    } finally {
      abort.closeRequest = true
      peer.close()
      clientNio.selector.keys().forEach(_.channel().close())
    }
  }
}
