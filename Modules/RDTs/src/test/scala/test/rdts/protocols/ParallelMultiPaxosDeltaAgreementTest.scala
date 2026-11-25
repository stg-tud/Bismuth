package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.Util.Agreement
import rdts.protocols.spanner.ParallelMultiPaxos
import rdts.protocols.{BallotNum, Participants, Paxos}

import scala.collection.mutable

/** Agreement of `spanner.ParallelMultiPaxos` when replicas exchange only the deltas that the
  * operations return, not their full states.
  *
  * Regression test. `ParallelMultiPaxos` used `rounds.size` as the slot after the highest known slot
  * (`nextSlot`, and the open slots `commitIndex + 1 until rounds.size` of `upkeep`). That is correct
  * only if the known slots are a prefix `0 until rounds.size`. Deltas can arrive in any order, so a
  * replica can know the slots 0, 2, 3 without slot 1. Then `rounds.size` is 3:
  * `proposeIfLeader(value)` proposed in the existing slot 3, and `upkeep` never treated slot 3 as
  * open. A vote in a leader election (phase 1b) is a promise for its slot and all later slots, but
  * `upkeep` reports the accepted values of the open slots only.
  *
  * On the trace below, C knows the slots 0, 2, 3 and is elected in ballot (C,1). Before the fix, C
  * got `z` chosen in slot 3 by C, B and D, and then voted for E in slot 2. That vote also made E
  * leader of slot 3, but it did not tell E that C accepted `z` there. E got `w` chosen in slot 3 in
  * ballot (E,2) by E, A and B.
  *
  * Now the slot after the highest known slot is the highest key plus one. Without a slot index
  * (`slotIndex = false`), C proposes `z` in the new slot 4 and E proposes `w` in slot 5. With
  * `proposeIfLeader(3, value)` (`slotIndex = true`), C still gets `z` chosen in slot 3, but slot 3
  * is open for C's `upkeep`, C reports `z` with its vote for E, and E proposes `z`.
  *
  * With full-state merges the known slots are always a prefix, and the trace is not possible.
  *
  * A second bug, still open, needs `startLeaderElection(index)`, see
  * [[ParallelMultiPaxosBallotReuseTest]].
  */
class ParallelMultiPaxosDeltaAgreementTest extends munit.FunSuite {

  type S = ParallelMultiPaxos[String]

  val names: List[String]       = List("A", "B", "C", "D", "E")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the final state of every replica, and the slots that C knows when it
    * proposes. With `slotIndex`, C and E propose in slot 3 with `proposeIfLeader(3, value)`;
    * otherwise they use `proposeIfLeader(value)`.
    */
  def runTrace(slotIndex: Boolean): (Map[String, S], Set[Long]) = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> ParallelMultiPaxos.empty[String]))
    // applies an operation at replica x and returns its delta
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        delta
    def deliver(delta: S, to: String*): Unit  = to.foreach(y => st(y) = Lattice.merge(st(y), delta))
    def elect(x: String): S                   = doOp(x)(_.startLeaderElection)
    def propose(x: String, value: String): S  = doOp(x)(_.proposeIfLeader(value))
    def propose3(x: String, value: String): S =
      if slotIndex then doOp(x)(_.proposeIfLeader(3, value)) else propose(x, value)
    def up(x: String): S = doOp(x)(_.upkeep)

    // A is elected in (A,0) in slot 0 by A, B, D and proposes in the slots 0 to 2
    deliver(elect("A"), "B", "D")
    deliver(up("B"), "A"); deliver(up("D"), "A")
    val p0 = propose("A", "x0"); val p1 = propose("A", "x1"); val p2 = propose("A", "x2")
    // B and D accept in the slots 0 and 1 and learn that these are decided (commitIndex 1)
    deliver(p0, "B", "D"); deliver(p1, "B", "D")
    val b01 = up("B"); val d01 = up("D")
    deliver(d01, "B"); deliver(b01, "D"); up("B"); up("D")
    // B and D accept in slot 2; only A learns that it is decided (commitIndex 2)
    deliver(p2, "B", "D")
    val b2 = up("B"); val d2 = up("D")
    deliver(b01, "A"); deliver(d01, "A"); deliver(b2, "A"); deliver(d2, "A"); up("A")
    // A starts an election in its next slot 3
    val a3 = elect("A")
    // C receives only the deltas of the slots 2 and 3. It starts elections in slot 0 until its
    // ballot (C,1) is above A's ballot (A,1). C now knows the slots 0, 2, 3.
    deliver(p2, "C"); deliver(a3, "C")
    elect("C")
    val c1 = elect("C")
    // B and D vote for C in their open slot 2
    deliver(c1, "B", "D")
    deliver(up("B"), "C"); deliver(up("D"), "C")
    // C proposes z; B and D accept: z is chosen in (C,1)
    val slotsOfC = st("C").rounds.keySet
    val cz       = propose3("C", "z")
    deliver(cz, "B", "D")
    val bz = up("B"); up("D")
    // E starts an election in (E,2); C and A vote for it
    deliver(c1, "E")
    val e2 = elect("E")
    deliver(e2, "C", "A")
    deliver(up("C"), "E"); deliver(up("A"), "E")
    // E proposes w; A and B accept what E proposed
    val ew = propose3("E", "w")
    deliver(ew, "A", "B")
    up("A"); deliver(up("B"), "A"); up("A")
    deliver(b2, "D"); deliver(bz, "D"); up("D")
    (st.toMap, slotsOfC)
  }

  /** The values that are chosen in `slot` in some ballot, according to the votes of all replicas. */
  def chosen(st: Map[String, S], slot: Long): Set[String] =
    st.values.flatMap(_.rounds.get(slot)).reduceOption(Lattice.merge).toList
      .flatMap(_.rounds.values).map(_.proposals.decision).collect {
        case Agreement.Decided(value) => value
      }.toSet

  test("trace precondition: order of the ballots") {
    val order                      = Paxos.given_Ordering_BallotNum
    def ballot(n: String, c: Long) = BallotNum(Uid.predefined(n), c)
    assert(order.gt(ballot("C", 1), ballot("A", 1)))
    assert(order.gt(ballot("E", 2), ballot("C", 1)))
  }

  test("trace precondition: C knows the slots 0, 2, 3 when it proposes") {
    assertEquals(runTrace(slotIndex = false)._2, Set(0L, 2L, 3L))
    assertEquals(runTrace(slotIndex = true)._2, Set(0L, 2L, 3L))
  }

  test("without slot index, C proposes in the slot after the highest known slot") {
    val st = runTrace(slotIndex = false)._1
    assertEquals(chosen(st, 3), Set.empty[String])
    assertEquals(chosen(st, 4), Set("z"))
  }

  test("with slot index, C reports the value it accepted in slot 3") {
    val st = runTrace(slotIndex = true)._1
    assertEquals(chosen(st, 3), Set("z"))
    assertEquals(st("A").log.get(3L), Some("z"))
    assertEquals(st("D").log.get(3L), Some("z"))
  }

  for slotIndex <- List(false, true) do {
    val variant = if slotIndex then "with slot index" else "without slot index"

    test(s"$variant: no slot has two chosen values") {
      val st = runTrace(slotIndex)._1
      for slot <- 0L to 5L do assert(chosen(st, slot).sizeIs <= 1, s"slot $slot: ${chosen(st, slot)}")
    }

    test(s"$variant: two replicas never log different values for the same slot") {
      val st = runTrace(slotIndex)._1
      for
          slot <- 0L to 5L
          x    <- names
          y    <- names
          vx   <- st(x).log.get(slot)
          vy   <- st(y).log.get(slot)
      do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
    }

    test(s"$variant: merging the states of two replicas does not throw") {
      val st = runTrace(slotIndex)._1
      for x <- names; y <- names do Lattice.merge(st(x), st(y))
    }
  }
}

/** Regression test for a bug of `spanner.ParallelMultiPaxos` with delta exchange, fixed by
  * `SlotElections.withReports`.
  *
  * `startLeaderElection(index)` takes the next ballot from the elections of the slots up to
  * `index`. A replica that first starts an election in slot 1 and then in slot 0 uses the same
  * ballot twice. A voter then votes for that ballot twice, in two deltas: first in slot 1, where it
  * reports the value it accepted in slot 1, and later in slot 0, where it has nothing to report. The
  * second delta alone makes the candidate leader of slot 1.
  *
  * On the trace below (3 replicas), `v` is chosen in slot 1 in ballot (A,0) by A and B. Before the
  * fix, C got `w` chosen in slot 1 in ballot (C,1) by C and B. Now the delta of B's vote in slot 0
  * also contains the round in which B accepted `v` in slot 1, so C proposes `v`. With full-state
  * merges the trace was always safe: B's state contains its vote for `v`.
  */
class ParallelMultiPaxosBallotReuseTest extends munit.FunSuite {

  type S = ParallelMultiPaxos[String]

  val names: List[String]       = List("A", "B", "C")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  def runTrace(): Map[String, S] = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> ParallelMultiPaxos.empty[String]))
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        delta
    def deliver(delta: S, to: String*): Unit = to.foreach(y => st(y) = Lattice.merge(st(y), delta))
    def up(x: String): S                     = doOp(x)(_.upkeep)

    // A is elected in (A,0) in slot 0 by A and B
    val a0 = doOp("A")(_.startLeaderElection)
    deliver(a0, "B", "C")
    deliver(up("B"), "A")
    // A proposes x0 in slot 0 and v in slot 1; B accepts v: chosen in (A,0). Slot 0 stays open at B.
    doOp("A")(_.proposeIfLeader("x0"))
    deliver(doOp("A")(_.proposeIfLeader("v")), "B")
    up("B")
    // C starts an election in slot 1, in ballot (C,1). B votes and reports v; the delta is lost.
    deliver(doOp("C")(_.startLeaderElection(1)), "B")
    up("B")
    // C starts an election in slot 0, again in ballot (C,1). B votes in slot 0 and reports nothing.
    deliver(doOp("C")(_.startLeaderElection(0)), "B")
    deliver(up("B"), "C")
    // C is leader of slot 1, knows no value there and proposes w; B accepts: chosen in (C,1)
    deliver(doOp("C")(_.proposeIfLeader(1, "w")), "B")
    up("B")
    st.toMap
  }

  /** The values that are chosen in `slot` in some ballot, according to the votes that `state` knows. */
  def chosen(state: S, slot: Long): Set[String] =
    state.rounds.get(slot).toList.flatMap(_.rounds.values).map(_.proposals.decision).collect {
      case Agreement.Decided(value) => value
    }.toSet

  test("trace precondition: v is chosen in slot 1") {
    assert(chosen(runTrace()("B"), 1).contains("v"))
  }

  test("two ballots never choose different values in one slot") {
    assertEquals(chosen(runTrace()("B"), 1), Set("v"))
  }
}
