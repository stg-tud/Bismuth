package channels.connection

import channels.connection.{Connection, ConnectionDescriptor, LatentConnection, QueuedLocalConnection, SynchronousLocalConnection}
import rdts.base.Uid

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver {
  def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]]
}

object ChannelResolver {
  def disconnected = new ChannelResolver {
    override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] = None
  }
}

case class PeerConnectInfo(uid: Uid, channelConnectors: Set[ConnectionDescriptor] = Set.empty)

class LocalConnectionRegistry(
    queued: collection.Map[String, QueuedLocalConnection],
    synchronous: collection.Map[String, SynchronousLocalConnection] = Map.empty,
) extends ChannelResolver {
  def queuedServer(details: ConnectionDescriptor): Option[LatentConnection[ConnectionDescriptor]] =
    details match
        case ConnectionDescriptor.QueuedLocal(id) => queued.get(id).map(_.server)
        case _                                    => None

  def synchronousServer(details: ConnectionDescriptor): Option[LatentConnection[ConnectionDescriptor]] =
    details match
        case ConnectionDescriptor.SynchronousLocal(id) => synchronous.get(id).map(_.server)
        case _                                         => None

  override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
    details match
        case ConnectionDescriptor.QueuedLocal(id)      => queued.get(id).map(_.client(id))
        case ConnectionDescriptor.SynchronousLocal(id) => synchronous.get(id).map(_.client(id))
        case _                                         => None
}
