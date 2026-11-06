package channels

import channels.connection.ConnectionDescriptor

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
