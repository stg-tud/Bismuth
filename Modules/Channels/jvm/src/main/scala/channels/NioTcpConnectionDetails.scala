package channels

import java.net.InetSocketAddress

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ChannelResolver {

  def listen(
      host: String = "127.0.0.1",
      port: Int = 0
  ): (ChannelConnectInfo.TcpWebSocket, LatentConnection) = {
    val socketFactory = nio.defaultServerSocketChannel(InetSocketAddress(host, port))
    val probe         = socketFactory()
    val actualPort    = probe.getLocalAddress.asInstanceOf[InetSocketAddress].getPort
    probe.close()
    (
      ChannelConnectInfo.TcpWebSocket(host, actualPort),
      nio.listen(nio.defaultServerSocketChannel(InetSocketAddress(host, actualPort)))
    )
  }

  override def connect(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.Tcp(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ChannelConnectInfo.TcpWebSocket(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case _ => None
}
