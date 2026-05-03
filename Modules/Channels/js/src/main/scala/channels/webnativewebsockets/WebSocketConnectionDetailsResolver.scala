package channels.webnativewebsockets

import channels.{ArrayMessageBuffer, ChannelConnectDescriptor, ChannelResolver, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}

/** vibecoded. dont trust 😉 */
class WebSocketConnectionDetailsResolver[T: JsonValueCodec] extends ChannelResolver[T] {
  override def canConnect(details: ChannelConnectDescriptor): Boolean =
    details match
        case ChannelConnectDescriptor.WebSocket(_) => true
        case ChannelConnectDescriptor.Tcp(_, _)    => true
        case _                                     => false

  private def jsonConnection(latent: LatentConnection[MessageBuffer], name: String): LatentConnection[T] =
    LatentConnection.adapt[MessageBuffer, T](
      mb => readFromArray[T](mb.asArray),
      value => ArrayMessageBuffer(writeToArray(value)),
      name
    )(latent)

  override def connect(details: ChannelConnectDescriptor, label: String): Option[LatentConnection[T]] =
    details match
        case ChannelConnectDescriptor.WebSocket(url) =>
          Some(jsonConnection(WebsocketConnect.connect(url), s"websocket-json:$label"))
        case ChannelConnectDescriptor.Tcp(host, port) =>
          Some(jsonConnection(WebsocketConnect.connect(s"ws://$host:$port"), s"websocket-json:$label"))
        case _ => None
}
