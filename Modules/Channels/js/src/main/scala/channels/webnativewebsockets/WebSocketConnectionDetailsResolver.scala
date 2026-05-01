package channels.webnativewebsockets

import channels.{ArrayMessageBuffer, ConnectionDetails, ConnectionDetailsResolver, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}

/** vibecoded. dont trust 😉 */
class WebSocketConnectionDetailsResolver[T: JsonValueCodec] extends ConnectionDetailsResolver[ConnectionDetails, T] {
  override def canConnect(details: ConnectionDetails): Boolean =
    details match
      case ConnectionDetails.WebSocket(_) => true
      case ConnectionDetails.Tcp(_, _)    => true
      case _                              => false


  private def jsonConnection(latent: LatentConnection[MessageBuffer], name: String): LatentConnection[T] =
    LatentConnection.adapt[MessageBuffer, T](
      mb => readFromArray[T](mb.asArray),
      value => ArrayMessageBuffer(writeToArray(value)),
      name
    )(latent)

  override def connect(details: ConnectionDetails, label: String): Option[LatentConnection[T]] =
    details match
      case ConnectionDetails.WebSocket(url) => Some(jsonConnection(WebsocketConnect.connect(url), s"websocket-json:$label"))
      case ConnectionDetails.Tcp(host, port) => Some(jsonConnection(WebsocketConnect.connect(s"ws://$host:$port"), s"websocket-json:$label"))
      case _                                 => None
}
