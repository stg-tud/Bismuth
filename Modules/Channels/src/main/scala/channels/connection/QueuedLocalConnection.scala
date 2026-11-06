package channels.connection

import de.rmgk.delay.{Async, Callback, Promise, Sync}

import scala.util.{Failure, Success, Try}

case class ConnectionClosedException(msg: String) extends Exception(msg)

/** A manually-driven message queue for local connections.
  *
  * `send` only enqueues message delivery. Messages are delivered when `deliverOne()` / `deliverAll()` is called.
  */
class LocalMessageQueue {
  private var queue: List[(receiver: Callback[MessageBuffer], message: Try[MessageBuffer])] = Nil

  // Delivered messages are logged (newest first) so tests can observe what was actually handed out.
  private var deliveredLog: List[(receiver: Callback[MessageBuffer], message: Try[MessageBuffer])] = Nil

  def enqueue(receiver: Callback[MessageBuffer], value: Try[MessageBuffer]): Unit = synchronized {
    queue = (receiver, value) :: queue
  }

  def elements: List[Try[MessageBuffer]] = synchronized(queue).map(_.message)

  /** All messages that have already been delivered, in delivery order. */
  def delivered: List[Try[MessageBuffer]] = synchronized(deliveredLog).reverse.map(_.message)

  def size: Int = synchronized(queue.size)

  def nonEmpty: Boolean = synchronized(queue.nonEmpty)

  /** Delivers all messages currently queued at method entry, in the order they were enqueued
    * (so messages on one connection keep their order, e.g. data before close, as with real transports).
    * Messages enqueued while delivering are kept for the next call.
    *
    * @return
    *   number of delivered messages
    */
  def deliverAll(): Int = {
    val toDeliver = synchronized {
      val current = queue
      queue = Nil
      current.reverse
    }

    toDeliver.foreach { case (receiver, value) =>
      synchronized { deliveredLog = (receiver, value) :: deliveredLog }
      receiver.complete(value)
    }
    toDeliver.size
  }

  /** Delivers the most recently enqueued message (LIFO).
    * Lets tests drive the exact interleaving of a message exchange.
    *
    * @return
    *   `true` if a message was delivered, `false` if the queue was empty
    */
  def deliverOne(): Boolean = {
    val toDeliver = synchronized {
      queue match
          case head :: tail =>
            queue = tail
            Some(head)
          case Nil => None
    }
    toDeliver.foreach { case (receiver, value) =>
      synchronized { deliveredLog = (receiver, value) :: deliveredLog }
      receiver.complete(value)
    }
    toDeliver.nonEmpty
  }
}

/** In-memory connections between a single “server” and multiple “clients”, for tests.
  *
  * Sending only enqueues the message on the [[LocalMessageQueue]]; it is delivered when the queue is told to deliver.
  * Each end looks up the receiver of the other end on delivery. Both ends are attached within `connect`, before any
  * message can be delivered, so each end can send right away.
  */
class QueuedLocalConnection(val serverId: String, val messageQueue: LocalMessageQueue = LocalMessageQueue()) {

  object server extends Listener {

    case class Establish(serverSendsOn: Connection, clientConnectionSendsTo: Promise[Callback[MessageBuffer]])
    val connectionEstablished: Promise[Callback[Establish]] = Promise()

    def listen(receiver: Receive): Async[Abort, ConnectionDescriptor.QueuedLocal] = Async.fromCallback[Establish] {
      connectionEstablished.succeed(Async.handler)
    }.map { connChan =>
      connChan.clientConnectionSendsTo.succeed(receiver.connectionEstablished(connChan.serverSendsOn))
      ConnectionDescriptor.QueuedLocal(serverId)
    }
  }

  /** One end of a connection: sends to the receiver of the other end, as soon as that one is attached. */
  private class End(name: String, to: Promise[Callback[MessageBuffer]]) extends Connection {
    private val deliver: Callback[MessageBuffer] = message =>
      to.async.run {
        case Success(receiver) => receiver.complete(message)
        case Failure(error)    => throw error
      }
    override def send(message: MessageBuffer): Async[Any, Unit] =
      Sync { messageQueue.enqueue(deliver, Success(message)) }
    override def close(): Unit    = messageQueue.enqueue(deliver, Failure(ConnectionClosedException("closed")))
    override def toString: String = name
  }

  def client(id: String): Connector = new Connector {

    def connect(receiver: Receive): Async[Abort, Connection] = Async {
      val toServerMessages = Promise[Callback[MessageBuffer]]()
      val toClientMessages = Promise[Callback[MessageBuffer]]()
      val toServer         = End(s"From[$id]", toServerMessages)
      val toClient         = End(s"To[$id]", toClientMessages)

      val established = server.connectionEstablished.async.bind
      established.succeed(server.Establish(toClient, toServerMessages))
      toClientMessages.succeed(receiver.connectionEstablished(toServer))
      toServer
    }
  }
}
