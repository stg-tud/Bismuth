package channels

import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver {
  def canConnect(details: ChannelConnectInfo): Boolean
  def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection]
}

object ChannelResolver {
  def disconnected =  new ChannelResolver {
    override def canConnect(details: ChannelConnectInfo): Boolean = false
    override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] = None
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

class LocalConnectionRegistry(queued: collection.Map[String, QueuedLocalConnection]) extends ChannelResolver {
  def queuedServer(details: ChannelConnectInfo): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                        => None

  override def canConnect(details: ChannelConnectInfo): Boolean =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.contains(id)
        case _                                        => false

  override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] =
    details match
        case ChannelConnectInfo.QueuedLocal(id) => queued.get(id).map(_.client(label))
        case _                                        => None
}
