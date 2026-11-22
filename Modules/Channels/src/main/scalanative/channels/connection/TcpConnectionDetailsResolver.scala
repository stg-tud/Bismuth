package channels.connection

import java.net.{InetSocketAddress, URI}
import scala.concurrent.ExecutionContext
import scala.util.Try

/** Resolves TCP and websocket-over-TCP descriptors, and `ws://` websocket URLs, using [[TCP]].
  * There is no TLS, thus no `wss://`.
  */
class TcpConnectionDetailsResolver(executionContext: ExecutionContext) extends ChannelResolver {

  override def connect(details: ConnectionDescriptor): Option[Connector] =
    details match
        case ConnectionDescriptor.Tcp(host, port) =>
          Some(TCP.connect(TCP.defaultSocket(InetSocketAddress(host, port)), executionContext))
        case ConnectionDescriptor.TcpWebSocket(host, port) =>
          Some(TCP.connect(TCP.defaultSocket(InetSocketAddress(host, port)), executionContext))
        case ConnectionDescriptor.WebSocket(url) =>
          Try(URI.create(url)).toOption.filter(_.getScheme == "ws").map { uri =>
            val host = uri.getHost.stripPrefix("[").stripSuffix("]")
            val port = if uri.getPort == -1 then 80 else uri.getPort
            val path = Option(uri.getRawPath).filter(_.nonEmpty).getOrElse("/") +
              Option(uri.getRawQuery).fold("")("?" + _)
            TCP.connectWebSocket(
              TCP.defaultSocket(InetSocketAddress(host, port)),
              uri.getRawAuthority, // the Host header: host, and port if given
              path,
              executionContext
            )
          }
        case _ => None
}
