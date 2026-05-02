package channels

import java.net.InetSocketAddress

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ChannelResolver[MessageBuffer] {
  override def canConnect(details: ChannelConnectDescriptor): Boolean =
    details match
      case ChannelConnectDescriptor.Tcp(_, _) => true
      case _                           => false

  def listen(host: String = "127.0.0.1", port: Int = 0): (ChannelConnectDescriptor.Tcp, LatentConnection[MessageBuffer]) = {
    val socketFactory = nio.defaultServerSocketChannel(InetSocketAddress(host, port))
    val probe         = socketFactory()
    val actualPort    = probe.getLocalAddress.asInstanceOf[InetSocketAddress].getPort
    probe.close()
    (ChannelConnectDescriptor.Tcp(host, actualPort), nio.listen(nio.defaultServerSocketChannel(InetSocketAddress(host, actualPort))))
  }

  override def connect(details: ChannelConnectDescriptor, label: String): Option[LatentConnection[MessageBuffer]] =
    details match
      case ChannelConnectDescriptor.Tcp(host, port) => Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
      case _                                 => None
}
