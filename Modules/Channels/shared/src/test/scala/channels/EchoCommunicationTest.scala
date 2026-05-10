package channels

import channels.TestUtil.printErrors
import de.rmgk.delay.{Async, Callback}

import java.util.concurrent.{ExecutorService, Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.Failure

trait EchoCommunicationTest[CD <: ConnectionDescriptor](
    serverConn: ExecutionContext => LatentConnection[CD],
    clientConn: ExecutionContext => CD => LatentConnection[Connection]
) extends munit.FunSuite {

  // need an execution context that generates new tasks as TCP does lots of blocking
  val executor: ExecutorService = Executors.newCachedThreadPool()
  val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)

  override def afterAll(): Unit =
    executor.shutdownNow()

  test("sample communication") {

    given abort: Abort = Abort()

    val toSend                 = List("Hi", "ho", "let’s", "go")
    val messageCounter         = Semaphore(0)
    var received: List[String] = Nil

    var traced = List.empty[String]

    def trace(msg: String) = synchronized {
      traced = msg :: traced
    }

    trace("test starting")

    val echoServer: Async[Abort, CD] =
      serverConn(ec).prepare: conn =>
          printErrors: mb =>
              trace("server received; echoing")
              conn.send(mb).runToFuture(())

    val client: Async[Abort, Connection] = Async { (_: Abort) ?=>
      val descriptor = echoServer.bind
      clientConn.apply(ec).apply(descriptor).prepare({ conn =>
        printErrors { mb =>
          trace("client received")
          synchronized {
            received = String(mb.asArray) :: received
          }
          messageCounter.release()
        }
      }).bind
    }

    val sending = Async: (_: Abort) ?=>
        trace("starting sending")
        val conn = client.bind
        trace("sending")
        ec.execute: () =>
            toSend.foreach: msg =>
                conn.send(ArrayMessageBuffer(msg.getBytes())).run:
                    printErrors(_ => ())

    sending.runIn(summon):
        printErrors: conn =>
            ()

    trace("test waiting")

    messageCounter.acquire(toSend.size)
    assertEquals(toSend.sorted, received.sorted, traced)
  }

}
