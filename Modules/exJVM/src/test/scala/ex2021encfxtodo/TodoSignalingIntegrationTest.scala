package ex2021encfxtodo

import channels.{Abort, ConcurrencyHelper, NioTCP, NioTcpConnectionDetailsResolver}
import javafx.application.Platform
import munit.FunSuite
import rdts.base.LocalUid
import replication.research.SignalingServer

import java.util.UUID
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

class TodoSignalingIntegrationTest extends FunSuite {

  private lazy val fxStarted: Unit = {
    val latch = CountDownLatch(1)
    try
        Platform.startup(() => latch.countDown())
        assert(latch.await(5, TimeUnit.SECONDS), "JavaFX platform did not start")
    catch
        case _: IllegalStateException => ()
  }

  final private class SignalingFixture {
    val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    val nioAbort    = Abort()
    val nioThread   = Executors.newSingleThreadExecutor()
    val nioResolver = new NioTcpConnectionDetailsResolver(nio)

    val (listenDetails, serverConn) = nioResolver.listen("127.0.0.1", 0)
    val signaling                   = SignalingServer(debug = false)
    signaling.addIncomingConnection(serverConn)
    nioThread.execute(() => nio.loopSelection(nioAbort))

    val connectionString: String = s"tcp://${listenDetails.host}:${listenDetails.port}"

    def stop(): Unit = {
      signaling.stop()
      nioAbort.closeRequest = true
      nio.selector.wakeup()
      nioThread.shutdownNow()
      ()
    }
  }

  private def eventually(timeoutMs: Long = 5000, sleepMs: Long = 20)(assertion: => Unit): Unit = {
    val deadline                = System.currentTimeMillis() + timeoutMs
    var last: Option[Throwable] = None
    while System.currentTimeMillis() < deadline do
        try
            assertion
            return
        catch
            case t: Throwable =>
              last = Some(t)
              Thread.sleep(sleepMs)
    last.foreach(throw _)
  }

  test("two todo states discover each other through signaling and replicate") {
    fxStarted

    val signaling = new SignalingFixture()
    val a         = new SyncedTodoListCrdt(LocalUid.gen())
    val b         = new SyncedTodoListCrdt(LocalUid.gen())

    try
        a.connect(signaling.connectionString)
        eventually() {
          assertEquals(signaling.signaling.topicPeers("encfx").size, 1)
        }

        b.connect(signaling.connectionString)
        eventually() {
          assertEquals(signaling.signaling.topicPeers("encfx").size, 2)
        }
        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        val id    = UUID.randomUUID()
        val entry = TodoEntry("via signaling")
        a.put(id, entry)

        eventually() {
          assertEquals(b.get(id), Some(entry))
        }
    finally
        a.shutdown()
        b.shutdown()
        signaling.stop()
  }
}
