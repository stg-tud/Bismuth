package channels

import channels.connection.{ChannelResolver, ConnectionDescriptor, Connector}

import java.net.{InetSocketAddress, UnixDomainSocketAddress}

class NioTcpConnectionDetailsResolver(nio: NioTCP) extends ChannelResolver {

  override def connect(details: ConnectionDescriptor): Option[Connector] =
    details match
        case ConnectionDescriptor.Tcp(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ConnectionDescriptor.TcpWebSocket(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ConnectionDescriptor.Unix(path) =>
          Some(nio.connect(nio.defaultSocketChannel(UnixDomainSocketAddress.of(path))))
        case _ => None
}
