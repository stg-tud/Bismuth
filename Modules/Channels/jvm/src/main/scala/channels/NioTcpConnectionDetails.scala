package channels

import java.net.InetSocketAddress

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ChannelResolver {
  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.Tcp(_, _) => true
        case _                            => false

  def listen(
      host: String = "127.0.0.1",
      port: Int = 0
  ): (ChannelConnectInfo.Tcp, LatentConnection) = {
    val socketFactory = nio.defaultServerSocketChannel(InetSocketAddress(host, port))
    val probe         = socketFactory()
    val actualPort    = probe.getLocalAddress.asInstanceOf[InetSocketAddress].getPort
    probe.close()
    (
      ChannelConnectInfo.Tcp(host, actualPort),
      nio.listen(nio.defaultServerSocketChannel(InetSocketAddress(host, actualPort)))
    )
  }

  override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.Tcp(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case _ => None
}
