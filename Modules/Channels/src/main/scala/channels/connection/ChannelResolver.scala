package channels.connection

import channels.connection.{ConnectionDescriptor, Connector, Listener, QueuedLocalConnection}
import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver {
  def connect(details: ConnectionDescriptor): Option[Connector]
}

object ChannelResolver {
  def disconnected: ChannelResolver = new ChannelResolver {
    override def connect(details: ConnectionDescriptor): Option[Connector] = None
  }
}

case class PeerConnectInfo(uid: Uid, channelConnectors: Set[ConnectionDescriptor] = Set.empty)

class LocalConnectionRegistry(
    queued: collection.Map[String, QueuedLocalConnection],
) extends ChannelResolver {
  def queuedServer(details: ConnectionDescriptor): Option[Listener] =
    details match
        case ConnectionDescriptor.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                    => None

  override def connect(details: ConnectionDescriptor): Option[Connector] =
    details match
        case ConnectionDescriptor.QueuedLocal(id) => queued.get(id).map(_.client(id))
        case _                                    => None
}
