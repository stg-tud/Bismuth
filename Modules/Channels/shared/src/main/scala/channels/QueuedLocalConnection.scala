package channels

import de.rmgk.delay.{Async, Callback, Promise}


/** A manually-driven message queue for local connections.
  *
  * `send` only enqueues message delivery. Messages are delivered when `deliverOne()` / `deliverAll()` is called.
  */
class LocalMessageQueue[T] {
  private var queue: List[(receiver: Callback[T], message: T)] = Nil

  def enqueue(receiver: Callback[T], value: T): Unit = synchronized {
    queue = (receiver, value) :: queue
  }

  def elements: List[T] = synchronized(queue).map(_.message)

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

    toDeliver.foreach { case (receiver, value) => receiver.succeed(value) }
    toDeliver.size
  }
}

/** Like [[SynchronousLocalConnection]], but message delivery is queued and manually executed. */
class QueuedLocalConnection[T](val messageQueue: LocalMessageQueue[T] = LocalMessageQueue[T]()) {

  object server extends LatentConnection[T] {

    case class Establish(serverSendsOn: Connection[T], clientConnectionSendsTo: Promise[Callback[T]])
    val connectionEstablished: Promise[Callback[Establish]] = Promise()

    def prepare(receiver: Receive[T]): Async[Abort, Connection[T]] = Async.fromCallback[Establish] {
      connectionEstablished.succeed(Async.handler)
    }.map { connChan =>
      connChan.clientConnectionSendsTo.succeed(receiver.messageHandler(connChan.serverSendsOn))
      connChan.serverSendsOn
    }
  }

  def client(id: String): LatentConnection[T] = new LatentConnection[T] {

    val toServerMessages: Promise[Callback[T]] = Promise()

    object toServer extends Connection[T] {
      def send(msg: T): Async[Any, Unit] = Async {
        val cb = toServerMessages.async.bind
        messageQueue.enqueue(cb, msg)
      }
      override def close(): Unit = ()
      override def toString: String = s"From[$id]"
    }

    def prepare(receiver: Receive[T]): Async[Abort, Connection[T]] = Async {
      val callback = receiver.messageHandler(toServer)

      val toClient = new Connection[T] {
        override def close(): Unit                      = ()
        override def send(message: T): Async[Any, Unit] = Async {
          messageQueue.enqueue(callback, message)
        }
        override def toString: String = s"To[$id]"
      }

      val established = server.connectionEstablished.async.bind
      established.succeed(server.Establish(toClient, toServerMessages))
      toServer
    }
  }
}
