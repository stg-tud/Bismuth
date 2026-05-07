package channels.webnativewebsockets

import channels.{ChannelConnectInfo, ChannelResolver, LatentConnection}

/** vibecoded */
class WebSocketConnectionDetailsResolver extends ChannelResolver {
  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.WebSocket(_) => true
        case _                               => false

  override def connect(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.WebSocket(url) => Some(WebsocketConnect.connect(url))
        case _                                 => None
}
