package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.knowledgeGroups.MultiPaxos as KnowledgeGroupMultiPaxos
import rdts.protocols.{BallotNum, Participants, Paxos}

import scala.collection.mutable

/** Agreement of `knowledgeGroups.MultiPaxos` when replicas exchange only the deltas that the
  * operations return, not their full states.
  *
  * Regression test. A vote in a leader election (phase 1b) is a promise for its slot and all later
  * slots, but `upkeep` runs phase 1b only in the open slots, and the phase-1b delta of a slot
  * carries only the values accepted in that slot. The log of this variant need not be a prefix: C
  * can log slot 2 while slot 0 is still open. C then votes for E in slot 0, and that vote also
  * makes E leader of slot 2. Before `SlotElections.withReports`, the vote did not tell E that C
  * accepted `true` in slot 2: `true` was chosen in slot 2 in ballot (A,0) by A, B and C, and E got
  * `false` chosen in slot 2 in ballot (E,0) by E, B and D.
  *
  * `upkeep` now adds to a delta with an election vote the newest accepted round of every known
  * slot above the vote, so C's vote reports `true` and E proposes `true` in slot 2.
  */
class KnowledgeGroupMultiPaxosDeltaAgreementTest extends munit.FunSuite {

  type S = KnowledgeGroupMultiPaxos[Boolean]

  val names: List[String]       = List("A", "B", "C", "D", "E")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the final state of every replica. */
  def runTrace(): Map[String, S] = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> KnowledgeGroupMultiPaxos.empty[Boolean]))
    // applies an operation at replica x and returns its delta
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        delta
    def deliver(delta: S, to: String*): Unit                 = to.foreach(y => st(y) = Lattice.merge(st(y), delta))
    def startLE(x: String, index: Long): S                   = doOp(x)(_.startLeaderElection(index))
    def propose(x: String, index: Long, value: Boolean): S   = doOp(x)(_.proposeIfLeader(index, value))
    def up(x: String): S                                     = doOp(x)(_.upkeep)

    // A is elected in (A,0) in slot 0 by A, B, C
    val a0 = startLE("A", 0)
    deliver(a0, "B", "C")
    deliver(up("B"), "A"); deliver(up("C"), "A")
    // A opens slots 0 and 1 and proposes true in slot 2
    propose("A", 0, false); propose("A", 1, false)
    deliver(propose("A", 2, true), "B", "C")
    deliver(up("B"), "C")
    up("C")                                         // C votes for true: chosen by A, B, C; C logs slot 2 -> true
    // E is elected in (E,0) in slot 0 by E, C, D. Slot 0 is still open at C, so C votes there.
    deliver(startLE("E", 0), "C", "D")
    deliver(up("C"), "E")                           // C's vote, without its vote for true in slot 2
    deliver(up("D"), "E")
    // E knows no value of slot 2 and proposes false there
    propose("E", 1, false)
    deliver(propose("E", 2, false), "B", "D")
    deliver(up("B"), "D")                           // B accepts the higher ballot (E,0)
    up("D")                                         // false chosen by E, B, D; D logs slot 2 -> false
    st.toMap
  }

  test("trace precondition: ballot (E,0) is above (A,0)") {
    assert(Paxos.given_Ordering_BallotNum.gt(BallotNum(Uid.predefined("E"), 0), BallotNum(Uid.predefined("A"), 0)))
  }

  test("trace precondition: C logs slot 2 while slot 0 is open") {
    val st = runTrace()
    assertEquals(st("C").log.get(2L), Some(true))
    assertEquals(st("C").log.get(0L), None)
  }

  test("two replicas never log different values for the same slot") {
    val st = runTrace()
    for
        slot <- 0L to 2L
        x    <- names
        y    <- names
        vx   <- st(x).log.get(slot)
        vy   <- st(y).log.get(slot)
    do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
  }

  test("merging the states of two replicas does not throw") {
    val st = runTrace()
    Lattice.merge(st("C"), st("D"))
  }
}
