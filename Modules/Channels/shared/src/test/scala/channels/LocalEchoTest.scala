package channels

import de.rmgk.delay.{Async, Callback, Sync}

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

final class ImmediateServer[CD <: ConnectionDescriptor](
    descriptor: CD,
    start: Receive => Async[Abort, ?]
) extends LatentConnection[CD] {
  override def prepare(receiver: Receive): Async[Abort, CD] = Async.fromCallback { abort ?=>
    start(receiver).runIn(abort) {
      case _ => ()
    }
    Async.handler.succeed(descriptor)
  }
}

class EchoServerTestSynchronousLocal extends EchoCommunicationTest[ConnectionDescriptor.SynchronousLocal](
      { _ =>
        EchoServerTestSynchronousLocal.current = SynchronousLocalConnection("echo-sync")
        ImmediateServer(
          ConnectionDescriptor.SynchronousLocal("echo-sync"),
          EchoServerTestSynchronousLocal.current.server.prepare
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
        ImmediateServer(
          ConnectionDescriptor.QueuedLocal("echo-queued"),
          EchoServerTestQueuedLocal.current.link.server.prepare
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
