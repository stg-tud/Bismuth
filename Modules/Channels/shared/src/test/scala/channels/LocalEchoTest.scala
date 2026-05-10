package channels

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

final class ImmediateServer[CD <: ConnectionDescriptor, Establish](
    descriptor: CD,
    register: Callback[Establish] => Unit,
    connect: (Receive, Establish) => Unit
) extends LatentConnection[CD] {
  override def prepare(receiver: Receive): Async[Abort, CD] = Sync {
    register(new Callback[Establish] {
      override def complete(result: scala.util.Try[Establish]): Unit = result match {
        case Success(establish) => connect(receiver, establish)
        case Failure(err)       => throw err
      }
    })
    descriptor
  }
}

class EchoServerTestSynchronousLocal extends EchoCommunicationTest[ConnectionDescriptor.SynchronousLocal](
      { _ =>
        EchoServerTestSynchronousLocal.current = SynchronousLocalConnection("echo-sync")
        val current = EchoServerTestSynchronousLocal.current
        ImmediateServer[ConnectionDescriptor.SynchronousLocal, current.server.Establish](
          ConnectionDescriptor.SynchronousLocal("echo-sync"),
          cb => current.server.connectionEstablished.succeed(cb),
          (receiver, establish) =>
            establish.clientConnectionSendsTo.succeed(receiver.connectionEstablished(establish.serverSendsOn))
        )
      },
      _ => _ => EchoServerTestSynchronousLocal.current.client("client")
    ) {
  override def supportsDisconnectDetection: Boolean = false
}

object EchoServerTestSynchronousLocal {
  var current: SynchronousLocalConnection = scala.compiletime.uninitialized
}

class EchoServerTestQueuedLocal extends EchoCommunicationTest[ConnectionDescriptor.QueuedLocal](
      { _ =>
        val queue = LocalMessageQueue()
        EchoServerTestQueuedLocal.current = EchoServerTestQueuedLocal.QueuedState(queue, QueuedLocalConnection("echo-queued", queue))
        val current = EchoServerTestQueuedLocal.current
        ImmediateServer[ConnectionDescriptor.QueuedLocal, current.link.server.Establish](
          ConnectionDescriptor.QueuedLocal("echo-queued"),
          cb => current.link.server.connectionEstablished.succeed(cb),
          (receiver, establish) =>
            establish.clientConnectionSendsTo.succeed(receiver.connectionEstablished(establish.serverSendsOn))
        )
      },
      _ => _ =>
        new LatentConnection[Connection] {
          override def prepare(receiver: Receive): Async[Abort, Connection] = {
            var wrapped: QueueDrainingConnection | Null = null
            EchoServerTestQueuedLocal.current.link.client("client").prepare(new Receive {
              override def connectionEstablished(answers: Connection): Callback[MessageBuffer] = {
                wrapped = QueueDrainingConnection(answers, EchoServerTestQueuedLocal.current.queue)
                receiver.connectionEstablished(wrapped.nn)
              }
            }).map(_ => wrapped.nn)
          }
        }
    )

object EchoServerTestQueuedLocal {
  final case class QueuedState(queue: LocalMessageQueue, link: QueuedLocalConnection)
  var current: QueuedState = scala.compiletime.uninitialized
}
