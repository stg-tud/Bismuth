package channels

import java.net.StandardProtocolFamily
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class DisconnectTest extends munit.FunSuite {

  lazy val isMacOS: Boolean         = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))
  override def munitIgnore: Boolean = isMacOS

  test("disconnect nioTCP") {

    val ec = ExecutionContext.global

    val socket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)

    socket.configureBlocking(false)

    // socket.bind(new InetSocketAddress("localhost", 0))

    val socketPath = domainSocketHelperNonensese("some-name-disconnect")

    socket.bind(socketPath)

    val serverNioTCP = new NioTCP

    val serverAbort = Abort()

    ec.execute { () =>
      TestUtil.printErrors(_ => ()).complete(
        Try(
          serverNioTCP.loopSelection(serverAbort)
        )
      )
    }

    val listen = serverNioTCP.listen(() => socket)

    listen.prepare(conn =>
      TestUtil.printErrors { mb =>
        conn.send(mb).run(TestUtil.printErrors(mb => ()))
      }
    ).runIn(Abort()) {
      case Success(_)  =>
      case Failure(ex) => throw ex
    }

    def socketChannel: SocketChannel = {
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(socketPath)
      channel.configureBlocking(false)
      channel
    }

    val clientNioTCP = new NioTCP
    ec.execute(() => clientNioTCP.loopSelection(Abort()))
    val connect = serverNioTCP.connect(() => socketChannel)

    connect.prepare { conn =>
      {
        case Success(mb) =>
        case Failure(ex) => assert(ex.isInstanceOf[NoMoreDataException])
      }
    }.runIn(Abort()) {
      case Success(conn) =>
        conn.send(ArrayMessageBuffer("Hi!".getBytes())).runIn(Abort()) { TestUtil.printErrors(mb => ()) }
        Thread.sleep(10)
        serverNioTCP.selector.keys().forEach(_.channel().close())
        Thread.sleep(10)

        conn.send(ArrayMessageBuffer("Hi 2!".getBytes())).runIn(Abort()) { TestUtil.printErrors(mb => ()) }
      case Failure(_) =>
    }

    assert(true)

  }
}
