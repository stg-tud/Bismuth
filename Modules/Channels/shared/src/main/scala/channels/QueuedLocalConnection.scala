package channels

import de.rmgk.delay.{Async, Callback, Promise}

import scala.util.{Failure, Success, Try}

case class ConnectionClosedException(msg: String) extends Exception(msg)

/** A manually-driven message queue for local connections.
  *
  * `send` only enqueues message delivery. Messages are delivered when `deliverOne()` / `deliverAll()` is called.
  */
class LocalMessageQueue {
  private var queue: List[(receiver: Callback[MessageBuffer], message: Try[MessageBuffer])] = Nil

  def enqueue(receiver: Callback[MessageBuffer], value: Try[MessageBuffer]): Unit = synchronized {
    queue = (receiver, value) :: queue
  }

  def elements: List[Try[MessageBuffer]] = synchronized(queue).map(_.message)

  def size: Int = synchronized(queue.size)

  def nonEmpty: Boolean = synchronized(queue.nonEmpty)

  /** Delivers all messages currently queued at method entry.
    * Messages enqueued while delivering are kept for the next call.
    *
    * @return
    *   number of delivered messages
    */
  def deliverAll(): Int = {
    val toDeliver = synchronized {
      val current = queue
      queue = Nil
      current
    }

    toDeliver.foreach { case (receiver, value) => receiver.complete(value) }
    toDeliver.size
  }
}

/** Like [[SynchronousLocalConnection]], but message delivery is queued and manually executed. */
class QueuedLocalConnection(val serverId: String, val messageQueue: LocalMessageQueue = LocalMessageQueue()) {

  object server extends LatentConnection[ConnectionDescriptor.QueuedLocal] {

    case class Establish(serverSendsOn: Connection, clientConnectionSendsTo: Promise[Callback[MessageBuffer]])
    val connectionEstablished: Promise[Callback[Establish]] = Promise()

    def prepare(receiver: Receive): Async[Abort, ConnectionDescriptor.QueuedLocal] = Async.fromCallback[Establish] {
      connectionEstablished.succeed(Async.handler)
    }.map { connChan =>
      connChan.clientConnectionSendsTo.succeed(receiver.connectionEstablished(connChan.serverSendsOn))
      ConnectionDescriptor.QueuedLocal(serverId)
    }
  }

  def client(id: String): LatentConnection[Connection] = new LatentConnection[Connection] {

    val toServerMessages: Promise[Callback[MessageBuffer]] = Promise()

    object toServer extends Connection {
      def send(msg: MessageBuffer): Async[Any, Unit] = Async {
        val cb = toServerMessages.async.bind
        messageQueue.enqueue(cb, Success(msg))
      }
      override def close(): Unit = Async {
        val cb = toServerMessages.async.bind
        messageQueue.enqueue(cb, Failure(ConnectionClosedException("closed")))
      }.runIn(()) { _ => () }
      override def toString: String = s"From[$id]"
    }

    def prepare(receiver: Receive): Async[Abort, Connection] = Async {
      val callback = receiver.connectionEstablished(toServer)

      val toClient = new Connection {
        override def close(): Unit = messageQueue.enqueue(callback, Failure(ConnectionClosedException("closed")))
        override def send(message: MessageBuffer): Async[Any, Unit] = Async {
          messageQueue.enqueue(callback, Success(message))
        }
        override def toString: String = s"To[$id]"
      }

      val established = server.connectionEstablished.async.bind
      established.succeed(server.Establish(toClient, toServerMessages))
      toServer
    }
  }
}
