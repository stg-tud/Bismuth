package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.Participants
import rdts.protocols.spanner.ParallelMultiPaxos

import scala.collection.mutable

/** The log of `spanner.ParallelMultiPaxos` when replicas exchange only the deltas that the operations
  * return, not their full states.
  *
  * Known bug, the property tests are marked `.fail`. The delta of `upkeep` carries the absolute
  * `commitIndex` of the sender, which is merged by maximum, but only the log entries that this call of
  * `upkeep` adds. A log entry is therefore sent in exactly one delta per replica that logs it, and a
  * replica that merges a delta with the entry already has it and does not log it again. A replica that
  * receives a later `upkeep` delta without the earlier one gets a `commitIndex` above slots it has not
  * logged. Its own `upkeep` logs only from `commitIndex + 1`, and the deltas of all replicas carry only
  * the slots above their own `commitIndex`, so no later delta fills the hole. Only the lost delta itself
  * or a full state does.
  *
  * `readSince(t)` reads the slots `t until log.size`, which assumes that the log is a prefix. With a hole
  * it skips entries: on the trace below C ends with the log `1 -> x1, 2 -> x2`, `read` returns `[x1]`
  * and `readSince(commitIndex)` returns nothing. `readDecisions` returns nothing, as C never learns that
  * slot 0 is decided.
  *
  * Trace (3 replicas): A is elected in slot 0 and proposes `x0`. B accepts and learns that `x0` is
  * chosen; its `upkeep` delta (`commitIndex` 0, log `0 -> x0`) reaches A but not C. A proposes `x1`, B
  * accepts and logs it; its `upkeep` delta (`commitIndex` 1, log `1 -> x1`) reaches A and C. From then on
  * every delta reaches every replica, and A gets `x2` chosen.
  *
  * With full-state messages the same schedule has no hole.
  */
class ParallelMultiPaxosDeltaLogTest extends munit.FunSuite {

  type S = ParallelMultiPaxos[String]

  val names: List[String]       = List("A", "B", "C")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the state of every replica right after C received B's second delta, and
    * at the end. With `fullState`, every message is the full state of the sender instead of the delta.
    */
  def runTrace(fullState: Boolean): (Map[String, S], Map[String, S]) = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> ParallelMultiPaxos.empty[String]))
    // applies an operation at replica x and returns the message it sends
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        if fullState then st(x) else delta
    def deliver(msg: S, to: String*): Unit = to.foreach(y => st(y) = Lattice.merge(st(y), msg))
    def propose(x: String, value: String): S = doOp(x)(_.proposeIfLeader(value))
    def up(x: String): S                     = doOp(x)(_.upkeep)

    // A is elected in slot 0 by A and B
    deliver(doOp("A")(_.startLeaderElection), "B", "C")
    deliver(up("B"), "A")
    // A proposes x0; B accepts and logs slot 0. Its delta is lost on the way to C.
    deliver(propose("A", "x0"), "B", "C")
    deliver(up("B"), "A")
    // A proposes x1; B accepts and logs slot 1. Its delta reaches A and C.
    deliver(propose("A", "x1"), "B", "C")
    deliver(up("B"), "A", "C")
    val afterHole = st.toMap
    // C runs upkeep; from now on every message reaches every replica
    for _ <- 1 to 2 do deliver(up("C"), "A", "B")
    deliver(propose("A", "x2"), "B", "C")
    for _ <- 1 to 3; x <- names do deliver(up(x), names.filter(_ != x)*)
    (afterHole, st.toMap)
  }

  test("trace precondition: A and B log x0, x1, x2") {
    val st = runTrace(fullState = false)._2
    for x <- List("A", "B") do
        assertEquals(st(x).log, Map(0L -> "x0", 1L -> "x1", 2L -> "x2"), x)
        assertEquals(st(x).read, List("x0", "x1", "x2"), x)
  }

  test("trace precondition: C has commitIndex 1 but no entry for slot 0") {
    val c = runTrace(fullState = false)._1("C")
    assertEquals(c.commitIndex, 1L)
    assertEquals(c.log, Map(1L -> "x1"))
  }

  test("trace precondition: C ends with the log 1 -> x1, 2 -> x2 and read = [x1]") {
    val c = runTrace(fullState = false)._2("C")
    assertEquals(c.commitIndex, 2L)
    assertEquals(c.log, Map(1L -> "x1", 2L -> "x2"))
    assertEquals(c.read, List("x1"))
    assertEquals(c.readSince(c.commitIndex), Nil)
  }

  test("trace precondition: with full-state messages every replica logs x0, x1, x2") {
    val st = runTrace(fullState = true)._2
    for x <- names do assertEquals(st(x).read, List("x0", "x1", "x2"), x)
  }

  def noHoles(st: Map[String, S]): Unit =
    for x <- names; slot <- 0L to st(x).commitIndex do
        assert(st(x).log.contains(slot), s"$x has commitIndex ${st(x).commitIndex} but no entry for slot $slot")

  test("a replica logs every slot up to its commitIndex, right after the hole appears".fail) {
    noHoles(runTrace(fullState = false)._1)
  }

  test("a replica logs every slot up to its commitIndex, after further upkeep and deltas".fail) {
    noHoles(runTrace(fullState = false)._2)
  }

  test("read returns the committed prefix at every replica".fail) {
    val st = runTrace(fullState = false)._2
    for x <- names do assertEquals(st(x).read, List("x0", "x1", "x2"), x)
  }
}
