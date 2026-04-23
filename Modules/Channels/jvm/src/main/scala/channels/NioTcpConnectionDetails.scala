package channels

import java.net.InetSocketAddress

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ConnectionDetailsResolver[ConnectionDetails, MessageBuffer] {
  def listen(host: String = "127.0.0.1", port: Int = 0): (ConnectionDetails.Tcp, LatentConnection[MessageBuffer]) = {
    val socketFactory = nio.defaultServerSocketChannel(InetSocketAddress(host, port))
    val probe         = socketFactory()
    val actualPort    = probe.getLocalAddress.asInstanceOf[InetSocketAddress].getPort
    probe.close()
    (ConnectionDetails.Tcp(host, actualPort), nio.listen(nio.defaultServerSocketChannel(InetSocketAddress(host, actualPort))))
  }

  override def connect(details: ConnectionDetails, label: String): Option[LatentConnection[MessageBuffer]] =
    details match
      case ConnectionDetails.Tcp(host, port) => Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
      case _                                 => None
}
