package channels

import java.net.InetSocketAddress

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ChannelResolver {

  override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
    details match
        case ConnectionDescriptor.Tcp(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ConnectionDescriptor.TcpWebSocket(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case _ => None
}
