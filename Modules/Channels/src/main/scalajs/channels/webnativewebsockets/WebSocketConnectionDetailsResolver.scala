package channels.webnativewebsockets

import channels.connection.{ChannelResolver, Connection, ConnectionDescriptor, LatentConnection}

/** vibecoded */
class WebSocketConnectionDetailsResolver extends ChannelResolver {

  override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
    details match
        case ConnectionDescriptor.WebSocket(url)           => Some(WebsocketConnect.connect(url))
        case ConnectionDescriptor.TcpWebSocket(host, port) => Some(WebsocketConnect.connect(s"ws://$host:$port"))
        case _                                             => None
}
