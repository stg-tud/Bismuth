package ex2021encfxtodo

import munit.FunSuite
import rdts.base.LocalUid

import java.util.UUID

class TodoSignalingIntegrationTest extends FunSuite {

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

  test("two todo states bootstrap directly and replicate") {
    val a = new SyncedTodoListCrdt(LocalUid.gen())
    val b = new SyncedTodoListCrdt(LocalUid.gen())

    try
        b.connect(a.address)
        eventually() {
          assert(
            a.remoteAddresses.nonEmpty || b.remoteAddresses.nonEmpty,
            s"a peers=${a.remoteAddresses}, b peers=${b.remoteAddresses}"
          )
        }

        val id    = UUID.randomUUID()
        val entry = TodoEntry("via bootstrap")
        a.put(id, entry)

        eventually() {
          assertEquals(b.get(id), Some(entry))
        }
    finally
        a.shutdown()
        b.shutdown()
  }

  test("synchronized updates to the same todo keep replicating") {
    val a = new SyncedTodoListCrdt(LocalUid.gen())
    val b = new SyncedTodoListCrdt(LocalUid.gen())

    try
        b.connect(a.address)

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
  }

  test("late join after many edits still converges and keeps replicating") {
    val a = new SyncedTodoListCrdt(LocalUid.gen())
    val b = new SyncedTodoListCrdt(LocalUid.gen())
    val c = new SyncedTodoListCrdt(LocalUid.gen())

    try
        b.connect(a.address)

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

        c.connect(a.address)
        eventually(timeoutMs = 8000) {
          assertEquals(c.get(shared), Some(TodoEntry("v8", completed = true)))
          assertEquals(c.get(extraA), Some(TodoEntry("extra-a")))
          assertEquals(c.get(extraB), Some(TodoEntry("extra-b", completed = true)))
        }

        val postJoin = TodoEntry("after-join", completed = false)
        c.put(shared, postJoin)
        eventually() {
          assertEquals(c.get(shared), Some(postJoin))
          assertEquals(a.get(shared), Some(postJoin))
          assertEquals(b.get(shared), Some(postJoin))
        }
    finally
        a.shutdown()
        b.shutdown()
        c.shutdown()
  }

  test("adding a replica after prior edits backfills history and keeps future replication working") {
    val a = new SyncedTodoListCrdt(LocalUid.gen())
    val b = new SyncedTodoListCrdt(LocalUid.gen())
    val c = new SyncedTodoListCrdt(LocalUid.gen())

    try
        b.connect(a.address)

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

        c.connect(a.address)

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
  }
}
