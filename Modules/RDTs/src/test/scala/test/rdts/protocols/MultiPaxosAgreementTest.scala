package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.{BallotNum, MultiPaxos, Participants, Paxos}

import scala.collection.mutable

/** Agreement of MultiPaxos: two replicas must never log different values for the same slot.
  *
  * When `upkeep` sees a decision, it opens the next slot with a copy of the leader election of
  * the local `newestBallotWithLeader`, so that leader can skip phase 1. Two replicas can copy
  * different elections, and a replica that enters the next slot by merging has lost its
  * phase-1b promises of the previous slot (the `Epoch` lattice drops the lower epoch). The
  * trace below uses only the public API and full-state merges: in slot 1, leader A gets `true`
  * chosen in ballot (A,0) and leader E gets `false` chosen in ballot (E,0).
  */
class MultiPaxosAgreementTest extends munit.FunSuite {

  type S = MultiPaxos[Boolean]
  val names: List[String]     = List("A", "B", "C", "D", "E")
  given Participants          = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** Runs the trace and returns the final state of every replica. */
  def runTrace(): Map[String, S] = {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> MultiPaxos[Boolean]()))
    def merge(x: String, y: String): Unit = st(x) = Lattice.merge(st(x), st(y))
    def doOp(x: String)(op: ReplicaId ?=> S => S): Unit =
      given ReplicaId = lid(x)
      st(x) = Lattice.merge(st(x), op(st(x)))
    def startLE(x: String): Unit             = doOp(x)(_.startLeaderElection)
    def propose(x: String, v: Boolean): Unit = doOp(x)(_.proposeIfLeader(v))
    def upkeep(x: String): Unit              = doOp(x)(_.upkeep)

    // Slot 0
    startLE("A")
    merge("B", "A"); upkeep("B"); merge("C", "A"); upkeep("C")
    merge("A", "B"); merge("A", "C")                           // A elected in (A,0)
    propose("A", false)
    merge("B", "A"); upkeep("B")
    merge("C", "B"); upkeep("C")                               // false decided; C opens slot 1 with (A,0)
    startLE("E")
    merge("D", "E"); upkeep("D"); merge("B", "E"); merge("B", "D"); upkeep("B")
    merge("E", "B")                                            // E elected in (E,0) by E, D, B
    propose("E", true)                                         // proposes false (B's accepted value)
    merge("D", "E"); upkeep("D"); merge("B", "D"); upkeep("B") // B opens slot 1 with (E,0)
    // Slot 1
    merge("A", "C"); propose("A", true)
    merge("C", "A"); upkeep("C"); merge("D", "C"); upkeep("D") // D logs slot 1 -> true
    merge("E", "B"); propose("E", false)
    merge("B", "E"); upkeep("B"); merge("C", "B"); upkeep("C") // C logs slot 1 -> false
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
        vx   <- st(x).log.get(slot)
        vy   <- st(y).log.get(slot)
    do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
  }

  test("merging the states of two replicas does not throw".fail) {
    val st = runTrace()
    Lattice.merge(st("C"), st("D"))
  }
}
