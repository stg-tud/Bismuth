package channels.webnativewebsockets

import channels.{ChannelConnectInfo, ChannelResolver, LatentConnection}

/** vibecoded */
class WebSocketConnectionDetailsResolver extends ChannelResolver {

  override def connect(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.WebSocket(url) => Some(WebsocketConnect.connect(url))
        case _                                 => None
}
