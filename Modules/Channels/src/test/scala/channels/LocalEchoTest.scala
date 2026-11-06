package channels

import channels.connection.{Abort, Connection, ConnectionDescriptor, ConnectionInfo, Connector, Listener, LocalMessageQueue, MessageBuffer, QueuedLocalConnection, Receive}
import de.rmgk.delay.{Async, Callback, Sync}

import scala.util.{Failure, Success}

final class QueueDrainingConnection(inner: Connection, queue: LocalMessageQueue) extends Connection {
  override def info: ConnectionInfo = inner.info

  private def drain(): Unit =
    while queue.nonEmpty do
        queue.deliverAll()
        ()

  override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
    inner.send(message).run(_ => ())
    drain()
  }

  override def close(): Unit = {
    inner.close()
    drain()
  }
}

object QueueDrainingConnection {

  /** Wraps the connection produced by `connector`, so that every send on it delivers all queued messages. */
  def drainingAfterSend(connector: Connector, queue: LocalMessageQueue): Connector = new Connector {
    override def connect(receiver: Receive): Async[Abort, Connection] = {
      var wrapped: QueueDrainingConnection | Null = null
      connector.connect(new Receive {
        override def connectionEstablished(answers: Connection): Callback[MessageBuffer] = {
          wrapped = QueueDrainingConnection(answers, queue)
          receiver.connectionEstablished(wrapped.nn)
        }
      }).map(_ => wrapped.nn)
    }
  }
}

final class ImmediateServer[Establish](
    descriptor: ConnectionDescriptor,
    register: Callback[Establish] => Unit,
    connect: (Receive, Establish) => Unit
) extends Listener {
  override def listen(receiver: Receive): Async[Abort, ConnectionDescriptor] = Sync {
    register(new Callback[Establish] {
      override def complete(result: scala.util.Try[Establish]): Unit = result match {
        case Success(establish) => connect(receiver, establish)
        case Failure(err)       => throw err
      }
    })
    descriptor
  }
}

class EchoServerTestQueuedLocal extends EchoCommunicationTest(
      { (_, _) =>
        val queue = LocalMessageQueue()
        EchoServerTestQueuedLocal.current =
          EchoServerTestQueuedLocal.QueuedState(queue, QueuedLocalConnection("echo-queued", queue))
        val current = EchoServerTestQueuedLocal.current
        ImmediateServer[current.link.server.Establish](
          ConnectionDescriptor.QueuedLocal("echo-queued"),
          cb => current.link.server.connectionEstablished.succeed(cb),
          (receiver, establish) =>
            establish.clientConnectionSendsTo.succeed(receiver.connectionEstablished(establish.serverSendsOn))
        )
      },
      (_, _) => {
        case ConnectionDescriptor.QueuedLocal("echo-queued") =>
          Some(QueueDrainingConnection.drainingAfterSend(
            EchoServerTestQueuedLocal.current.link.client("client"),
            EchoServerTestQueuedLocal.current.queue
          ))
        case _ => None
      }
    )

object EchoServerTestQueuedLocal {
  final case class QueuedState(queue: LocalMessageQueue, link: QueuedLocalConnection)
  var current: QueuedState = scala.compiletime.uninitialized
}
