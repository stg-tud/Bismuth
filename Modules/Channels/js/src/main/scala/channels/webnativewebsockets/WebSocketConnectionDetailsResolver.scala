package channels.webnativewebsockets

import channels.{ChannelConnectInfo, ChannelResolver, LatentConnection}

/** vibecoded. dont trust 😉 */
class WebSocketConnectionDetailsResolver extends ChannelResolver {
  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.WebSocket(_) => true
        case ChannelConnectInfo.Tcp(_, _)    => true
        case _                                     => false

  override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.WebSocket(url) =>
          Some(WebsocketConnect.connect(url))
        case ChannelConnectInfo.Tcp(host, port) =>
          Some(WebsocketConnect.connect(s"ws://$host:$port"))
        case _ => None
}
