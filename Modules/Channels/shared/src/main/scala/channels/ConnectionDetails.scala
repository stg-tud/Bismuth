package channels

trait ConnectionDetailsResolver[D, T] {
  def connect(details: D, label: String): Option[LatentConnection[T]]
}

object ConnectionDetailsResolver {
  def orElse[D, T](resolvers: ConnectionDetailsResolver[D, T]*): ConnectionDetailsResolver[D, T] =
    new ConnectionDetailsResolver[D, T] {
      override def connect(details: D, label: String): Option[LatentConnection[T]] =
        resolvers.iterator.map(_.connect(details, label)).collectFirst { case Some(value) => value }
    }
}

enum ConnectionDetails {
  case Tcp(host: String, port: Int)
  case QueuedLocal(id: String)
  case SynchronousLocal(id: String)
}

class LocalConnectionRegistry[T] extends ConnectionDetailsResolver[ConnectionDetails, T] {
  private var queued: Map[String, QueuedLocalConnection[T]]       = Map.empty
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

  override def connect(details: ConnectionDetails, label: String): Option[LatentConnection[T]] = synchronized {
    details match
      case ConnectionDetails.QueuedLocal(id)      => queued.get(id).map(_.client(label))
      case ConnectionDetails.SynchronousLocal(id) => synchronous.get(id).map(_.client(label))
      case ConnectionDetails.Tcp(_, _)            => None
  }
}
