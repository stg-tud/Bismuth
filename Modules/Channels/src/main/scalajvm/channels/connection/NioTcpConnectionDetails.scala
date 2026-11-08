package channels
package connection

import channels.connection.{ChannelResolver, ConnectionDescriptor, Connector}

import java.net.{InetSocketAddress, UnixDomainSocketAddress}

/** Resolves TCP, websocket-over-TCP and unix socket descriptors, and [[ConnectionDescriptor.TcpTls]] if `tls` is given. */
class NioTcpConnectionDetailsResolver(nio: NioTCP, tls: Option[NioTls] = None) extends ChannelResolver {

  override def connect(details: ConnectionDescriptor): Option[Connector] =
    details match
        case ConnectionDescriptor.Tcp(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ConnectionDescriptor.TcpWebSocket(host, port) =>
          Some(nio.connect(nio.defaultSocketChannel(InetSocketAddress(host, port))))
        case ConnectionDescriptor.TcpTls(host, port) =>
          tls.map(tls => nio.connectTls(nio.defaultSocketChannel(InetSocketAddress(host, port)), tls))
        case ConnectionDescriptor.Unix(path) =>
          Some(nio.connect(nio.defaultSocketChannel(UnixDomainSocketAddress.of(path))))
        case _ => None
}
