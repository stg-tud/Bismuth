package channels

import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver[T] {
  def canConnect(details: ChannelConnectInfo): Boolean
  def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection[T]]
}

object ChannelResolver {
  def disconnected[T] =  new ChannelResolver[T] {
    override def canConnect(details: ChannelConnectInfo): Boolean = false
    override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection[T]] = None
  }
}

case class PeerConnectInfo(uid: Uid, channelConnectors: Set[ChannelConnectInfo])

enum ChannelConnectInfo {
  case Tcp(host: String, port: Int)
  case WebSocket(url: String)
  case WebRtc(peerId: String)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)
}

class LocalConnectionRegistry[T](queued: collection.Map[String, QueuedLocalConnection[T]]) extends ChannelResolver[T] {
  def queuedServer(details: ChannelConnectInfo): Option[LatentConnection[T]] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                        => None

  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.contains(id)
        case _                                        => false

  override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection[T]] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.client(label))
        case _                                        => None
}
