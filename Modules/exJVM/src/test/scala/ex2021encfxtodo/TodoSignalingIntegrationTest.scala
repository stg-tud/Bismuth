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

  test("synchronized updates to the same todo keep replicating") {
    fxStarted

    val signaling = new SignalingFixture()
    val a         = new SyncedTodoListCrdt(LocalUid.gen())
    val b         = new SyncedTodoListCrdt(LocalUid.gen())

    try
        a.connect(signaling.connectionString)
        b.connect(signaling.connectionString)

        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        val id = UUID.randomUUID()
        a.put(id, TodoEntry("initial"))
        eventually() {
          assertEquals(b.get(id), Some(TodoEntry("initial")))
        }

        a.put(id, TodoEntry("edited by a"))
        eventually() {
          assertEquals(b.get(id), Some(TodoEntry("edited by a")))
        }

        b.put(id, TodoEntry("edited by b", completed = true))
        eventually() {
          assertEquals(a.get(id), Some(TodoEntry("edited by b", completed = true)))
        }
    finally
        a.shutdown()
        b.shutdown()
        signaling.stop()
  }

  test("late join after many edits still converges and keeps replicating") {
    fxStarted

    val signaling = new SignalingFixture()
    val a         = new SyncedTodoListCrdt(LocalUid.gen())
    val b         = new SyncedTodoListCrdt(LocalUid.gen())
    val c         = new SyncedTodoListCrdt(LocalUid.gen())

    try
        a.connect(signaling.connectionString)
        b.connect(signaling.connectionString)

        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        val shared = UUID.randomUUID()
        a.put(shared, TodoEntry("v0"))
        eventually() {
          assertEquals(b.get(shared), Some(TodoEntry("v0")))
        }

        (1 to 8).foreach { i =>
          val entry = TodoEntry(s"v$i", completed = i % 2 == 0)
          if i % 2 == 0 then a.put(shared, entry) else b.put(shared, entry)
          eventually() {
            assertEquals(a.get(shared), Some(entry))
            assertEquals(b.get(shared), Some(entry))
          }
        }

        val extraA = UUID.randomUUID()
        val extraB = UUID.randomUUID()
        a.put(extraA, TodoEntry("extra-a"))
        b.put(extraB, TodoEntry("extra-b", completed = true))
        eventually() {
          assertEquals(a.get(extraB), Some(TodoEntry("extra-b", completed = true)))
          assertEquals(b.get(extraA), Some(TodoEntry("extra-a")))
        }

        c.connect(signaling.connectionString)

        eventually(timeoutMs = 8000) {
          assertEquals(c.get(shared), Some(TodoEntry("v8", completed = true)))
          assertEquals(c.get(extraA), Some(TodoEntry("extra-a")))
          assertEquals(c.get(extraB), Some(TodoEntry("extra-b", completed = true)))
        }
        eventually(timeoutMs = 8000) {
          assert(
            c.remoteAddresses.nonEmpty && (a.remoteAddresses.size >= 2 || b.remoteAddresses.size >= 2),
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}, c peers=${c.remoteAddresses}"
          )
        }

        val postJoin = TodoEntry("after-join", completed = false)
        c.put(shared, postJoin)
        assertEquals(c.get(shared), Some(postJoin))
        eventually() {
          assertEquals(c.get(shared), Some(postJoin))
          assertEquals(a.get(shared), Some(postJoin))
          assertEquals(b.get(shared), Some(postJoin))
        }
    finally
        a.shutdown()
        b.shutdown()
        c.shutdown()
        signaling.stop()
  }

  test("adding a replica after prior edits backfills history and keeps future replication working") {
    fxStarted

    val signaling = new SignalingFixture()
    val a         = new SyncedTodoListCrdt(LocalUid.gen())
    val b         = new SyncedTodoListCrdt(LocalUid.gen())
    val c         = new SyncedTodoListCrdt(LocalUid.gen())

    try
        a.connect(signaling.connectionString)
        b.connect(signaling.connectionString)

        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        val id1 = UUID.randomUUID()
        val id2 = UUID.randomUUID()
        a.put(id1, TodoEntry("before c joins 1"))
        b.put(id2, TodoEntry("before c joins 2", completed = true))

        eventually() {
          assertEquals(a.get(id2), Some(TodoEntry("before c joins 2", completed = true)))
          assertEquals(b.get(id1), Some(TodoEntry("before c joins 1")))
        }

        c.connect(signaling.connectionString)

        eventually() {
          assertEquals(c.get(id1), Some(TodoEntry("before c joins 1")))
          assertEquals(c.get(id2), Some(TodoEntry("before c joins 2", completed = true)))
        }

        val id3 = UUID.randomUUID()
        c.put(id3, TodoEntry("after c joined"))
        eventually() {
          assertEquals(a.get(id3), Some(TodoEntry("after c joined")))
          assertEquals(b.get(id3), Some(TodoEntry("after c joined")))
        }

        a.put(id1, TodoEntry("edited after c joined", completed = true))
        eventually() {
          assertEquals(b.get(id1), Some(TodoEntry("edited after c joined", completed = true)))
          assertEquals(c.get(id1), Some(TodoEntry("edited after c joined", completed = true)))
        }
    finally
        a.shutdown()
        b.shutdown()
        c.shutdown()
        signaling.stop()
  }

  test("reconnecting to the signaling server does not break existing replication") {
    fxStarted

    val signaling = new SignalingFixture()
    val a         = new SyncedTodoListCrdt(LocalUid.gen())
    val b         = new SyncedTodoListCrdt(LocalUid.gen())

    try
        a.connect(signaling.connectionString)
        b.connect(signaling.connectionString)

        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        (1 to 8).foreach { _ =>
          a.connect(signaling.connectionString)
          b.connect(signaling.connectionString)
        }

        eventually() {
          assertEquals(signaling.signaling.topicPeers("encfx").size, 2)
        }

        val idA    = UUID.randomUUID()
        val entryA = TodoEntry("after reconnect a->b")
        a.put(idA, entryA)
        eventually() {
          assertEquals(b.get(idA), Some(entryA))
        }

        val idB    = UUID.randomUUID()
        val entryB = TodoEntry("after reconnect b->a")
        b.put(idB, entryB)
        eventually() {
          assertEquals(a.get(idB), Some(entryB))
        }
    finally
        a.shutdown()
        b.shutdown()
        signaling.stop()
  }
}
