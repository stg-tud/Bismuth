package channels

/** vibecoded as part of the hyparview experiments */

trait ChannelResolver[T] {
  def canConnect(details: ChannelConnectDescriptor): Boolean
  def connect(details: ChannelConnectDescriptor, label: String): Option[LatentConnection[T]]
}

enum ChannelConnectDescriptor {
  case Tcp(host: String, port: Int)
  case WebSocket(url: String)
  case WebRtc(peerId: String)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)
}

class LocalConnectionRegistry[T](queued: collection.Map[String, QueuedLocalConnection[T]]) extends ChannelResolver[T] {
  def queuedServer(details: ChannelConnectDescriptor): Option[LatentConnection[T]] =
    details match
      case ChannelConnectDescriptor.QueuedLocal(id) => queued.get(id).map(_.server)
      case _                                        => None

  override def canConnect(details: ChannelConnectDescriptor): Boolean =
    details match
      case ChannelConnectDescriptor.QueuedLocal(id) => queued.contains(id)
      case _                                        => false

  override def connect(details: ChannelConnectDescriptor, label: String): Option[LatentConnection[T]] =
    details match
      case ChannelConnectDescriptor.QueuedLocal(id) => queued.get(id).map(_.client(label))
      case _                                        => None
}
