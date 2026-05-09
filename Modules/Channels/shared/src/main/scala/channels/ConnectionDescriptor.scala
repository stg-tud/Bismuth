package channels

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}

import java.net.URI
import scala.util.Try

enum ConnectionDescriptor {
  case Tcp(host: String, port: Int)
  case Udp(host: String, port: Int)
  case Unix(path: String)
  case TcpWebSocket(host: String, port: Int)
  case WebSocket(url: String)
  case WebRtc(peerId: String)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)

  def asUrl: String = this match
      case ConnectionDescriptor.Tcp(host, port)          => s"tcp://$host:$port"
      case ConnectionDescriptor.Udp(host, port)          => s"udp://$host:$port"
      case ConnectionDescriptor.Unix(path)               => s"unix://$path"
      case ConnectionDescriptor.TcpWebSocket(host, port) => s"tcp-ws://$host:$port"
      case ConnectionDescriptor.WebSocket(url)           => url
      case ConnectionDescriptor.WebRtc(peerId)           => s"webrtc://$peerId"
      case ConnectionDescriptor.QueuedLocal(id)          => s"queue://$id"
      case ConnectionDescriptor.SynchronousLocal(id)     => s"sync://$id"

  override def toString: String = asUrl
}

object ConnectionDescriptor {
  def parse(value: String): Option[ConnectionDescriptor] =
    Try(URI.create(value)).toOption.flatMap { uri =>
      uri.getScheme match
          case "tcp" =>
            for
                host <- Option(uri.getHost)
                port <- Option.when(uri.getPort >= 0)(uri.getPort)
            yield Tcp(host, port)
          case "udp" =>
            for
                host <- Option(uri.getHost)
                port <- Option.when(uri.getPort >= 0)(uri.getPort)
            yield Udp(host, port)
          case "unix" =>
            Option(uri.getHost)
              .orElse(Option(uri.getPath).filter(_.nonEmpty).map(_.stripPrefix("/")))
              .map(Unix.apply)
          case "tcp+ws" | "tcp-ws" =>
            for
                host <- Option(uri.getHost)
                port <- Option.when(uri.getPort >= 0)(uri.getPort)
            yield TcpWebSocket(host, port)
          case "ws" | "wss" => Some(WebSocket(value))
          case "webrtc" =>
            Option(uri.getHost)
              .orElse(Option(uri.getAuthority))
              .orElse(Option(uri.getSchemeSpecificPart).map(_.stripPrefix("//")).filter(_.nonEmpty))
              .map(WebRtc.apply)
          case "queue" =>
            Option(uri.getHost)
              .orElse(Option(uri.getAuthority))
              .orElse(Option(uri.getSchemeSpecificPart).map(_.stripPrefix("//")).filter(_.nonEmpty))
              .map(QueuedLocal.apply)
          case "sync" =>
            Option(uri.getHost)
              .orElse(Option(uri.getAuthority))
              .orElse(Option(uri.getSchemeSpecificPart).map(_.stripPrefix("//")).filter(_.nonEmpty))
              .map(SynchronousLocal.apply)
          case _ => None
    }

  given codec: JsonValueCodec[ConnectionDescriptor] with {
    override def decodeValue(in: JsonReader, default: ConnectionDescriptor): ConnectionDescriptor = {
      val value = in.readString(null.asInstanceOf[String])
      parse(value).getOrElse(in.decodeError(s"Invalid channel connect info: $value"))
    }

    override def encodeValue(x: ConnectionDescriptor, out: JsonWriter): Unit =
      out.writeVal(x.asUrl)

    override def nullValue: ConnectionDescriptor = null.asInstanceOf[ConnectionDescriptor]
  }
}
