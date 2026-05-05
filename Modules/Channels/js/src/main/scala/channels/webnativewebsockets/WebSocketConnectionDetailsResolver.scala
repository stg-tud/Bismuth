package channels.webnativewebsockets

import channels.{ArrayMessageBuffer, ChannelConnectInfo, ChannelResolver, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}

/** vibecoded. dont trust 😉 */
class WebSocketConnectionDetailsResolver[T: JsonValueCodec] extends ChannelResolver[T] {
  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.WebSocket(_) => true
        case ChannelConnectInfo.Tcp(_, _)    => true
        case _                                     => false

  private def jsonConnection(latent: LatentConnection[MessageBuffer], name: String): LatentConnection[T] =
    LatentConnection.adapt[MessageBuffer, T](
      mb => readFromArray[T](mb.asArray),
      value => ArrayMessageBuffer(writeToArray(value)),
      name
    )(latent)

  override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection[T]] =
    details match
        case ChannelConnectInfo.WebSocket(url) =>
          Some(jsonConnection(WebsocketConnect.connect(url), s"websocket-json:$label"))
        case ChannelConnectInfo.Tcp(host, port) =>
          Some(jsonConnection(WebsocketConnect.connect(s"ws://$host:$port"), s"websocket-json:$label"))
        case _ => None
}
