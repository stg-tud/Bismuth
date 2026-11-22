package channels

import channels.connection.{ConnectionDescriptor, *}

import java.net.{DatagramSocket, InetSocketAddress, StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.ServerSocketChannel
import java.nio.file.Files

class EchoServerTestUDP extends EchoCommunicationTest(
      (ec, _) => {
        // don’t do this normally, but we need a free random socket
        val ds = new DatagramSocket()
        UDP.listen(() => ds, ec)
      },
      (ec, _) => {
        case ConnectionDescriptor.Udp(host, port) =>
          Some(UDP.connect(InetSocketAddress(host, port), () => new DatagramSocket(), ec))
        case _ => None
      }
    ) {
  override def supportsDisconnectDetection: Boolean = false
}

def domainSocketHelperNonensese(name: String): UnixDomainSocketAddress = {
  val tmpPath    = Files.createTempDirectory("channels-test")
  val socketPath = tmpPath.resolve(name)
  socketPath.toFile.deleteOnExit()
  tmpPath.toFile.deleteOnExit()
  UnixDomainSocketAddress.of(socketPath)
}

class EchoServerTestNioTCP extends EchoCommunicationTest(
      { (ec, abort) =>
        val socket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)

        socket.configureBlocking(false)

        val socketPath = domainSocketHelperNonensese("some-name")

        socket.bind(socketPath)

        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

        ec.execute(() => nioTCP.loopSelection(abort))

        nioTCP.listen(bindsocket = () => socket)
      },
      (ec, abort) => {
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        NioTcpConnectionDetailsResolver(nioTCP)
      }
    )

/** TCP over IPv6 (the loopback address), with the default channels of [[NioTCP]]. */
class EchoServerTestNioTCPv6 extends EchoCommunicationTest(
      { (ec, abort) =>
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        nioTCP.listen(nioTCP.defaultServerSocketChannel(InetSocketAddress(java.net.InetAddress.getByName("::1"), 0)))
      },
      (ec, abort) => {
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        NioTcpConnectionDetailsResolver(nioTCP)
      }
    )

/** The websocket client of [[NioTCP]], connecting to the websocket server of [[NioTCP]]. */
class EchoServerTestNioTCPWebSocketClient extends EchoCommunicationTest(
      { (ec, abort) =>
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        nioTCP.listen(nioTCP.defaultServerSocketChannel(InetSocketAddress(java.net.InetAddress.getLoopbackAddress, 0)))
      },
      (ec, abort) => {
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        {
          case ConnectionDescriptor.TcpWebSocket(host, port) =>
            Some(nioTCP.connectWebSocket(nioTCP.defaultSocketChannel(InetSocketAddress(host, port)), host))
          case _ => None
        }
      }
    )
