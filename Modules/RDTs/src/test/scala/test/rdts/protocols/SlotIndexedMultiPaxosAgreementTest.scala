package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.knowledgeGroups.MultiPaxos as KnowledgeGroupMultiPaxos
import rdts.protocols.spanner.ParallelMultiPaxos
import rdts.protocols.{BallotNum, Participants, Paxos}

import scala.collection.mutable

/** Agreement of the slot-indexed MultiPaxos variants: two replicas must never log different values
  * for the same slot.
  *
  * Known bug, the tests are marked `.fail`. `proposeIfLeader(index, value)` opens a new slot with a
  * copy of the leader election of the local `newestBallotWithLeader` of slot `index - 1`, so that
  * leader can skip phase 1. Two replicas can copy different elections, and the new slot holds no
  * phase-1b promises: a replica that voted for the higher ballot in slot 0 can still accept a
  * proposal of the lower ballot in slot 1. On the trace below, which uses only the public API and
  * full-state merges, leader A gets `true` chosen in ballot (A,0) and leader E gets `false` chosen
  * in ballot (E,0), both in slot 1.
  *
  * This is the trace of [[MultiPaxosAgreementTest]] with explicit slot indices. `rdts.protocols.MultiPaxos`
  * avoids the bug by sharing the leader elections between all slots.
  */
abstract class SlotIndexedMultiPaxosAgreementTest[S: Lattice] extends munit.FunSuite {

  def empty: S
  def startLeaderElection(s: S, index: Long)(using ReplicaId): S
  def proposeIfLeader(s: S, index: Long, value: Boolean)(using ReplicaId, Participants): S
  def upkeep(s: S)(using ReplicaId, Participants): S
  def log(s: S): Map[Long, Boolean]

  val names: List[String]       = List("A", "B", "C", "D", "E")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the final state of every replica. */
  def runTrace(): Map[String, S] = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> empty))
    def merge(x: String, y: String): Unit = st(x) = Lattice.merge(st(x), st(y))
    def doOp(x: String)(op: ReplicaId ?=> S => S): Unit =
      given ReplicaId = lid(x)
      st(x) = Lattice.merge(st(x), op(st(x)))
    def startLE(x: String, index: Long): Unit             = doOp(x)(startLeaderElection(_, index))
    def propose(x: String, index: Long, v: Boolean): Unit = doOp(x)(proposeIfLeader(_, index, v))
    def up(x: String): Unit                               = doOp(x)(upkeep(_))

    // Slot 0
    startLE("A", 0)
    merge("B", "A"); up("B"); merge("C", "A"); up("C")
    merge("A", "B"); merge("A", "C")                   // A elected in (A,0)
    propose("A", 0, false)
    merge("B", "A"); up("B")
    merge("C", "B"); up("C")                           // false decided
    startLE("E", 0)
    merge("D", "E"); up("D"); merge("B", "E"); merge("B", "D"); up("B")
    merge("E", "B")                                    // E elected in (E,0) by E, D, B
    propose("E", 0, true)                              // proposes false (B's accepted value)
    merge("D", "E"); up("D"); merge("B", "D"); up("B")
    // Slot 1
    merge("A", "C"); propose("A", 1, true)             // A opens slot 1 with the election of (A,0)
    merge("C", "A"); up("C"); merge("D", "C"); up("D") // D logs slot 1 -> true
    merge("E", "B"); propose("E", 1, false)            // E opens slot 1 with the election of (E,0)
    merge("B", "E"); up("B"); merge("C", "B"); up("C") // C logs slot 1 -> false
    st.toMap
  }

  test("trace precondition: ballot (E,0) is above (A,0)") {
    assert(Paxos.given_Ordering_BallotNum.gt(BallotNum(Uid.predefined("E"), 0), BallotNum(Uid.predefined("A"), 0)))
  }

  test("two replicas never log different values for the same slot".fail) {
    val st = runTrace()
    for
        slot <- 0L to 1L
        x    <- names
        y    <- names
        vx   <- log(st(x)).get(slot)
        vy   <- log(st(y)).get(slot)
    do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
  }

  test("merging the states of two replicas does not throw".fail) {
    val st = runTrace()
    Lattice.merge(st("C"), st("D"))
  }
}

class ParallelMultiPaxosAgreementTest extends SlotIndexedMultiPaxosAgreementTest[ParallelMultiPaxos[Boolean]] {
  type S = ParallelMultiPaxos[Boolean]
  def empty: S                                                                             = ParallelMultiPaxos.empty
  def startLeaderElection(s: S, index: Long)(using ReplicaId): S                           = s.startLeaderElection(index)
  def proposeIfLeader(s: S, index: Long, value: Boolean)(using ReplicaId, Participants): S =
    s.proposeIfLeader(index, value)
  def upkeep(s: S)(using ReplicaId, Participants): S = s.upkeep
  def log(s: S): Map[Long, Boolean]                  = s.log
}

class KnowledgeGroupMultiPaxosAgreementTest
    extends SlotIndexedMultiPaxosAgreementTest[KnowledgeGroupMultiPaxos[Boolean]] {
  type S = KnowledgeGroupMultiPaxos[Boolean]
  def empty: S                                                                             = KnowledgeGroupMultiPaxos.empty
  def startLeaderElection(s: S, index: Long)(using ReplicaId): S                           = s.startLeaderElection(index)
  def proposeIfLeader(s: S, index: Long, value: Boolean)(using ReplicaId, Participants): S =
    s.proposeIfLeader(index, value)
  def upkeep(s: S)(using ReplicaId, Participants): S = s.upkeep
  def log(s: S): Map[Long, Boolean]                  = s.log
}
