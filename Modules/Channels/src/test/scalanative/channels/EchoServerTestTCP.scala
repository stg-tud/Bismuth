package channels

import channels.connection.{ConnectionDescriptor, TCP, TcpConnectionDetailsResolver}

import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}

class EchoServerTestTCP extends EchoCommunicationTest(
      { (ec, _) =>
        val socket = new ServerSocket

        try socket.setReuseAddress(true)
        catch {
          case _: SocketException =>
          // some implementations may not allow SO_REUSEADDR to be set
        }

        socket.bind(new InetSocketAddress(InetAddress.getByName("localhost"), 0))

        TCP.listen(() => socket, ec)
      },
      (ec, _) => TcpConnectionDetailsResolver(ec)
    )

/** Native websocket client and server, connecting via a `ws://` URL of the [[TCP]] listener. */
class EchoServerTestTCPWebSocketClient extends EchoCommunicationTest(
      { (ec, _) =>
        TCP.listen(TCP.defaultServerSocket(new InetSocketAddress(InetAddress.getLoopbackAddress, 0)), ec)
      },
      (ec, _) => {
        val resolver = TcpConnectionDetailsResolver(ec)
        {
          case ConnectionDescriptor.TcpWebSocket(host, port) =>
            resolver.connect(ConnectionDescriptor.WebSocket(s"ws://${ConnectionDescriptor.urlHost(host)}:$port/path?q"))
          case _ => None
        }
      }
    )

class TcpConnectionDetailsResolverTest extends munit.FunSuite {
  val resolver = TcpConnectionDetailsResolver(scala.concurrent.ExecutionContext.global)

  test("resolves ws, but not wss or other urls") {
    assert(resolver.connect(ConnectionDescriptor.WebSocket("ws://localhost:1234/path")).isDefined)
    assert(resolver.connect(ConnectionDescriptor.WebSocket("ws://localhost")).isDefined)
    assert(resolver.connect(ConnectionDescriptor.WebSocket("wss://localhost:1234/path")).isEmpty)
    assert(resolver.connect(ConnectionDescriptor.WebSocket("http://localhost:1234/path")).isEmpty)
    assert(resolver.connect(ConnectionDescriptor.WebSocket("not a url")).isEmpty)
  }
}
