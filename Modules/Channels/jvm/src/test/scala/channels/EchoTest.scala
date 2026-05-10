package channels

import com.sun.net.httpserver.HttpServer

import java.net.http.HttpClient
import java.net.{DatagramSocket, InetSocketAddress, StandardProtocolFamily, URI, UnixDomainSocketAddress}
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.Files

class EchoServerTestUDP extends EchoCommunicationTest[ConnectionDescriptor.Udp](
      ec => {
        // don’t do this normally, but we need a free random socket
        val ds = new DatagramSocket()
        UDP.listen(() => ds, ec)
      },
      ec => descriptor => UDP.connect(InetSocketAddress(descriptor.host, descriptor.port), () => new DatagramSocket(), ec)
    ) {
  override def supportsDisconnectDetection: Boolean = false
}

class EchoServerTestSunJavaHTTP extends EchoCommunicationTest[ConnectionDescriptor.WebSocket](
      _ => {

        val server = HttpServer.create()

        server.bind(InetSocketAddress("127.0.0.1", 0), 0)
        val port = server.getAddress.getPort

        val handler = JavaHttp.SSEServer(
          handler => server.createContext("/path", handler),
          ConnectionDescriptor.WebSocket(s"http://127.0.0.1:$port/path")
        )

        server.start()
        handler

      },
      ec => descriptor => {
        val client = HttpClient.newHttpClient()
        JavaHttp.SSEClient(client, URI.create(descriptor.url), ec)
      }
    ) {
  override def supportsMultipleConnections: Boolean    = false
  override def supportsDisconnectDetection: Boolean    = false
  override def supportsStableConnectionObject: Boolean = false
}

def domainSocketHelperNonensese(name: String) = {
  val tmpPath    = Files.createTempDirectory("channels-test")
  val socketPath = tmpPath.resolve(name)
  socketPath.toFile.deleteOnExit()
  tmpPath.toFile.deleteOnExit()
  UnixDomainSocketAddress.of(socketPath)
}

class EchoServerTestNioTCP extends EchoCommunicationTest[ConnectionDescriptor.TcpWebSocket | ConnectionDescriptor.Unix](
      { ec =>
        val socket = ServerSocketChannel.open(StandardProtocolFamily.UNIX)

        socket.configureBlocking(false)

        val socketPath = domainSocketHelperNonensese("some-name")

        socket.bind(socketPath)

        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

        ec.execute(() => nioTCP.loopSelection(Abort()))

        nioTCP.listen(bindsocket = () => socket)
      },
      ec => {
        case descriptor: ConnectionDescriptor.Unix =>
          def socketChannel: SocketChannel = {
            val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
            channel.connect(UnixDomainSocketAddress.of(descriptor.path))
            channel.configureBlocking(false)
            channel
          }

          val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

          ec.execute(() => nioTCP.loopSelection(Abort()))

          nioTCP.connect(() => socketChannel)
        case descriptor: ConnectionDescriptor.TcpWebSocket =>
          val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

          ec.execute(() => nioTCP.loopSelection(Abort()))

          nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(descriptor.host, descriptor.port)))
      }
    )
