package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.{BallotNum, MultiPaxos, Participants, Paxos}

import scala.collection.mutable

/** Agreement of `MultiPaxos` when replicas exchange only the deltas that the operations return, not
  * their full states.
  *
  * Regression test. A delta that contained only votes in a leader election used to be sent with an
  * empty `Epoch` (slot 0) instead of the voter's current slot. A phase-1b vote reports only the
  * values accepted in the voter's current slot, so a replica still in an earlier slot counted the
  * vote there without learning the value the voter had accepted in that slot. On the trace below,
  * `v` is chosen in slot 0 in ballot (A,0) by A and B, B moves to slot 1, and its vote for C reaches
  * C while C is still in slot 0. C then got `w` chosen in slot 0 in ballot (C,0) by C and A.
  *
  * Now the vote carries B's slot 1, so C and A move to slot 1 when they receive it.
  */
class MultiPaxosDeltaAgreementTest extends munit.FunSuite {

  type S = MultiPaxos[String]

  val names: List[String]       = List("A", "B", "C")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the final state of every replica. */
  def runTrace(): Map[String, S] = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> MultiPaxos.empty[String]))
    // applies an operation at replica x and returns its delta
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        delta
    def deliver(delta: S, to: String*): Unit = to.foreach(y => st(y) = Lattice.merge(st(y), delta))

    // A is elected in (A,0) by A and B, and v is chosen in slot 0 by A and B
    deliver(doOp("A")(_.startLeaderElection), "B")
    deliver(doOp("B")(_.upkeep), "A")
    deliver(doOp("A")(_.proposeIfLeader("v")), "B")
    doOp("B")(_.upkeep)                              // B logs slot 0 -> v and moves to slot 1; delta lost
    // C, which knows nothing, starts an election in (C,0); B votes for it from slot 1
    val c1 = doOp("C")(_.startLeaderElection)
    deliver(c1, "B")
    val b2 = doOp("B")(_.upkeep)
    deliver(b2, "C")                                 // C is elected by B and C
    val c2 = doOp("C")(_.proposeIfLeader("w"))
    deliver(c1, "A"); deliver(b2, "A"); deliver(c2, "A")
    doOp("A")(_.upkeep)                              // before the fix: A logs slot 0 -> w
    st.toMap
  }

  test("trace precondition: ballot (C,0) is above (A,0)") {
    assert(Paxos.given_Ordering_BallotNum.gt(BallotNum(Uid.predefined("C"), 0), BallotNum(Uid.predefined("A"), 0)))
  }

  test("trace precondition: B logs slot 0 -> v") {
    assertEquals(runTrace()("B").log.get(0L), Some("v"))
  }

  test("two replicas never log different values for the same slot") {
    val st = runTrace()
    for
        slot <- 0L to 1L
        x    <- names
        y    <- names
        vx   <- st(x).log.get(slot)
        vy   <- st(y).log.get(slot)
    do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
  }

  test("merging the states of two replicas does not throw") {
    val st = runTrace()
    Lattice.merge(st("A"), st("B"))
  }
}
