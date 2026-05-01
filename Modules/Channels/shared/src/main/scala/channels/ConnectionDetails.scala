package channels

/** vibecoded as part of the hyparview experiments */

trait ConnectionDetailsResolver[D, T] {
  def canConnect(details: D): Boolean
  def connect(details: D, label: String): Option[LatentConnection[T]]
}

object ConnectionDetailsResolver {
  def orElse[D, T](resolvers: ConnectionDetailsResolver[D, T]*): ConnectionDetailsResolver[D, T] =
    new ConnectionDetailsResolver[D, T] {
      override def canConnect(details: D): Boolean =
        resolvers.exists(_.canConnect(details))

      override def connect(details: D, label: String): Option[LatentConnection[T]] =
        resolvers.iterator.map(_.connect(details, label)).collectFirst { case Some(value) => value }
    }

  def many[D, T](resolver: ConnectionDetailsResolver[D, T]): ConnectionDetailsResolver[Set[D], T] =
    new ConnectionDetailsResolver[Set[D], T] {
      override def canConnect(details: Set[D]): Boolean =
        details.exists(resolver.canConnect)

      override def connect(details: Set[D], label: String): Option[LatentConnection[T]] =
        details.iterator.map(resolver.connect(_, label)).collectFirst { case Some(value) => value }
    }
}

enum ConnectionDetails {
  case Tcp(host: String, port: Int)
  case WebSocket(url: String)
  case WebRtc(signalingUrl: String, peerId: String)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)
}

object ConnectionDetails {
  def describe(details: ConnectionDetails): String = details match
    case Tcp(host, port)              => s"tcp:$host:$port"
    case WebSocket(url)               => s"ws:$url"
    case WebRtc(signalingUrl, peerId) => s"webrtc:$peerId@$signalingUrl"
    case QueuedLocal(id)              => s"queued:$id"
    case SynchronousLocal(id)         => s"sync:$id"
}

class LocalConnectionRegistry[T] extends ConnectionDetailsResolver[ConnectionDetails, T] {
  private var queued: Map[String, QueuedLocalConnection[T]]            = Map.empty
  private var synchronous: Map[String, SynchronousLocalConnection[T]] = Map.empty

  def registerQueued(id: String, connection: QueuedLocalConnection[T] = QueuedLocalConnection[T]()): ConnectionDetails = synchronized {
    queued = queued.updated(id, connection)
    ConnectionDetails.QueuedLocal(id)
  }

  def registerSynchronous(
      id: String,
      connection: SynchronousLocalConnection[T] = SynchronousLocalConnection[T]()
  ): ConnectionDetails = synchronized {
    synchronous = synchronous.updated(id, connection)
    ConnectionDetails.SynchronousLocal(id)
  }

  def queuedServer(details: ConnectionDetails): Option[LatentConnection[T]] = synchronized {
    details match
      case ConnectionDetails.QueuedLocal(id) => queued.get(id).map(_.server)
      case _                                 => None
  }

  def synchronousServer(details: ConnectionDetails): Option[LatentConnection[T]] = synchronized {
    details match
      case ConnectionDetails.SynchronousLocal(id) => synchronous.get(id).map(_.server)
      case _                                      => None
  }

  override def canConnect(details: ConnectionDetails): Boolean = synchronized {
    details match
      case ConnectionDetails.QueuedLocal(id)      => queued.contains(id)
      case ConnectionDetails.SynchronousLocal(id) => synchronous.contains(id)
      case ConnectionDetails.Tcp(_, _)            => false
      case ConnectionDetails.WebSocket(_)         => false
      case ConnectionDetails.WebRtc(_, _)         => false
  }

  override def connect(details: ConnectionDetails, label: String): Option[LatentConnection[T]] = synchronized {
    details match
      case ConnectionDetails.QueuedLocal(id)      => queued.get(id).map(_.client(label))
      case ConnectionDetails.SynchronousLocal(id) => synchronous.get(id).map(_.client(label))
      case ConnectionDetails.Tcp(_, _)            => None
      case ConnectionDetails.WebSocket(_)         => None
      case ConnectionDetails.WebRtc(_, _)         => None
  }
}
