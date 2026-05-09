package channels

import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver {
  def connect(details: ConnectionDescriptor): Option[LatentConnection]
}

object ChannelResolver {
  def disconnected = new ChannelResolver {
    override def connect(details: ConnectionDescriptor): Option[LatentConnection] = None
  }
}

case class PeerConnectInfo(uid: Uid, channelConnectors: Set[ConnectionDescriptor])





class LocalConnectionRegistry(queued: collection.Map[String, QueuedLocalConnection]) extends ChannelResolver {
  def queuedServer(details: ConnectionDescriptor): Option[LatentConnection] =
    details match
        case ConnectionDescriptor.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                  => None

  override def connect(details: ConnectionDescriptor): Option[LatentConnection] =
    details match
        case ConnectionDescriptor.QueuedLocal(id) => queued.get(id).map(_.client(id))
        case _                                  => None
}
