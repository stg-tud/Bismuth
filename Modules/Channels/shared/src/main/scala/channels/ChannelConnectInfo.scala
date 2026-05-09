package channels

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import rdts.base.Uid

import java.net.URI
import scala.util.Try

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver {
  def connect(details: ChannelConnectInfo): Option[LatentConnection]
}

object ChannelResolver {
  def disconnected = new ChannelResolver {
    override def connect(details: ChannelConnectInfo): Option[LatentConnection] = None
  }
}

case class PeerConnectInfo(uid: Uid, channelConnectors: Set[ChannelConnectInfo])

enum ChannelConnectInfo {
  case Tcp(host: String, port: Int)
  case TcpWebSocket(host: String, port: Int)
  case WebSocket(url: String)
  case WebRtc(peerId: String)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)

  def asUrl: String = this match
    case ChannelConnectInfo.Tcp(host, port)             => s"tcp://$host:$port"
    case ChannelConnectInfo.TcpWebSocket(host, port)    => s"tcp+ws://$host:$port"
    case ChannelConnectInfo.WebSocket(url)              => url
    case ChannelConnectInfo.WebRtc(peerId)              => s"webrtc://$peerId"
    case ChannelConnectInfo.QueuedLocal(id)             => s"queue://$id"
    case ChannelConnectInfo.SynchronousLocal(id)        => s"sync://$id"

  override def toString: String = asUrl
}

object ChannelConnectInfo {
  def parse(value: String): Option[ChannelConnectInfo] =
    Try(URI.create(value)).toOption.flatMap { uri =>
      uri.getScheme match
        case "tcp" =>
          for
            host <- Option(uri.getHost)
            port <- Option.when(uri.getPort >= 0)(uri.getPort)
          yield Tcp(host, port)
        case "tcp+ws" =>
          for
            host <- Option(uri.getHost)
            port <- Option.when(uri.getPort >= 0)(uri.getPort)
          yield TcpWebSocket(host, port)
        case "ws" | "wss" => Some(WebSocket(value))
        case "webrtc"       => Option(uri.getHost).map(WebRtc.apply)
        case "queue"        => Option(uri.getHost).map(QueuedLocal.apply)
        case "sync"         => Option(uri.getHost).map(SynchronousLocal.apply)
        case _               => None
    }

  given codec: JsonValueCodec[ChannelConnectInfo] with {
    override def decodeValue(in: JsonReader, default: ChannelConnectInfo): ChannelConnectInfo = {
      val value = in.readString(null.asInstanceOf[String])
      parse(value).getOrElse(in.decodeError(s"Invalid channel connect info: $value"))
    }

    override def encodeValue(x: ChannelConnectInfo, out: JsonWriter): Unit =
      out.writeVal(x.asUrl)

    override def nullValue: ChannelConnectInfo = null.asInstanceOf[ChannelConnectInfo]
  }
}

class LocalConnectionRegistry(queued: collection.Map[String, QueuedLocalConnection]) extends ChannelResolver {
  def queuedServer(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                  => None

  override def connect(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.client(id))
        case _                                  => None
}
