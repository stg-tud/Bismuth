package channels

import channels.TestUtil.printErrors
import de.rmgk.delay.{Async, Callback}

import java.util.concurrent.{ExecutorService, Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.Failure

trait EchoCommunicationTest[Info](
    serverConn: ExecutionContext => (Info, LatentConnection[ConnectionDescriptor]),
    clientConn: ExecutionContext => Info => LatentConnection[Connection]
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

    val (info, serverLatent) = serverConn(ec)

    val echoServer: Async[Abort, ConnectionDescriptor] =
      serverLatent.prepare: conn =>
          printErrors: mb =>
              trace("server received; echoing")
              conn.send(mb).runToFuture(())

    val client: Async[Abort, Connection] =
      clientConn.apply(ec).apply(info).prepare: conn =>
          printErrors: mb =>
              trace("client received")
              synchronized {
                received = String(mb.asArray) :: received
              }
              messageCounter.release()

    echoServer.runIn(summon):
        printErrors: conn =>
            ()

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
