package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.Util.Agreement
import rdts.protocols.knowledgeGroups.MultiPaxos as KnowledgeGroupMultiPaxos
import rdts.protocols.{BallotNum, Participants, Paxos}

import scala.collection.mutable

/** Ballot reuse in `knowledgeGroups.MultiPaxos` when replicas exchange only the deltas that the
  * operations return, not their full states.
  *
  * Known bug, the agreement tests are marked `.fail`. It is the bug of
  * [[ParallelMultiPaxosBallotReuseTest]]. `startLeaderElection(index)` takes the next ballot from
  * `SlotElections.view(slots, index)`, which contains only the elections of the slots up to
  * `index`. A replica that first starts an election in slot 1 and then in slot 0 uses the same
  * ballot twice. A voter then votes for that ballot twice, in two deltas: first in slot 1, where it
  * reports the value it accepted in slot 1, and later in slot 0, where it has nothing to report.
  * `upkeep` does not vote again in slot 1, because the voter already voted for that ballot there.
  * The second delta alone makes the candidate leader of slot 1, because a vote in slot 0 counts for
  * all later slots.
  *
  * On the trace below (3 replicas), `v` is chosen in slot 1 in ballot (A,0) by A and B, and C gets
  * `w` chosen in slot 1 in ballot (C,1) by C and A. A logs `w` and B logs `v` for slot 1.
  *
  * The trace does not use the bug of [[KnowledgeGroupMultiPaxosDeltaAgreementTest]], where a voter
  * has already logged a slot and so does not report it. The only voter for C is A, and A has not
  * logged slot 1 when it votes in slot 1 and in slot 0 (B's vote for `v` never reaches A). If C's
  * second election used a fresh ballot (C,2), `upkeep` at A would vote in slot 1 as well and report
  * `v`, and C would propose `v`.
  *
  * With full-state merges the trace is safe: A's state contains its vote for `v` in slot 1, and C
  * proposes `v`.
  *
  * The only caller in the repository, `ex2026keyValuePrdtBench.MultiPaxosReplica`, calls
  * `startLeaderElection(state.log.size)`. The log only grows, so the index never goes down at a
  * replica, and with an index that does not go down the ballot always grows. That caller therefore
  * does not reuse a ballot.
  */
class KnowledgeGroupMultiPaxosBallotReuseTest extends munit.FunSuite {

  type S = KnowledgeGroupMultiPaxos[String]

  val names: List[String]       = List("A", "B", "C")
  given Participants            = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  /** The result of a run: the final states, the ballots of C's elections in slot 1 and slot 0, and
    * whether A had logged slot 1 when it voted in slot 1 and when it voted in slot 0.
    */
  case class Run(states: Map[String, S], ballotsOfC: List[BallotNum], aLoggedSlot1: List[Boolean])

  /** Runs the trace. With `fullState`, every message carries the sender's full state instead of
    * the delta of the operation.
    */
  def runTrace(fullState: Boolean = false): Run = {
    val st: mutable.Map[String, S] =
      mutable.LinkedHashMap.from(names.map(_ -> KnowledgeGroupMultiPaxos.empty[String]))
    // applies an operation at replica x and returns what x sends
    def doOp(x: String)(op: ReplicaId ?=> S => S): S =
        given ReplicaId = lid(x)
        val delta       = op(st(x))
        st(x) = Lattice.merge(st(x), delta)
        if fullState then st(x) else delta
    def deliver(msg: S, to: String*): Unit = to.foreach(y => st(y) = Lattice.merge(st(y), msg))
    def startLE(x: String, index: Long): S = doOp(x)(_.startLeaderElection(index))
    def propose(x: String, index: Long, value: String): S = doOp(x)(_.proposeIfLeader(index, value))
    def up(x: String): S                   = doOp(x)(_.upkeep)
    def ballotOfC(slot: Long): BallotNum   = st("C").slots(slot).rounds.keySet.filter(_.uid == Uid.predefined("C")).max(using Paxos.given_Ordering_BallotNum)

    // A is elected in (A,0) in slot 0 by A and B
    deliver(startLE("A", 0), "B", "C")
    deliver(up("B"), "A")
    // A proposes v in slot 1; B accepts: v chosen in (A,0). B's vote never reaches A.
    deliver(propose("A", 1, "v"), "B")
    up("B")
    // C starts an election in slot 1, in ballot (C,1). A votes in slot 1 and reports v; the
    // message is lost.
    deliver(startLE("C", 1), "A")
    val logged1 = st("A").log.contains(1L)
    up("A")
    // C starts an election in slot 0, again in ballot (C,1). A votes in slot 0 and reports nothing.
    deliver(startLE("C", 0), "A")
    val logged0 = st("A").log.contains(1L)
    deliver(up("A"), "C")
    // C is leader of slot 1, knows no value there and proposes w; A accepts: w chosen in (C,1)
    deliver(propose("C", 1, "w"), "A")
    deliver(up("A"), "C")
    up("C")
    Run(st.toMap, List(ballotOfC(1), ballotOfC(0)), List(logged1, logged0))
  }

  /** The values that are chosen in `slot` in some ballot, according to the votes in `states`. */
  def chosen(states: Iterable[S], slot: Long): Set[String] =
    states.toList.flatMap(_.slots.get(slot)).reduceOption(Lattice.merge[Paxos[String]]).toList
      .flatMap(_.rounds.values).map(_.proposals.decision).collect {
        case Agreement.Decided(value) => value
      }.toSet

  test("trace precondition: C uses ballot (C,1) in both elections, above (A,0)") {
    val c1 = BallotNum(Uid.predefined("C"), 1)
    assertEquals(runTrace().ballotsOfC, List(c1, c1))
    assert(Paxos.given_Ordering_BallotNum.gt(c1, BallotNum(Uid.predefined("A"), 0)))
  }

  test("trace precondition: A has not logged slot 1 when it votes for C") {
    assertEquals(runTrace().aLoggedSlot1, List(false, false))
  }

  test("trace precondition: v is chosen in slot 1 and B logs it") {
    val st = runTrace().states
    assert(chosen(List(st("B")), 1).contains("v"))
    assertEquals(st("B").log.get(1L), Some("v"))
  }

  test("two ballots never choose different values in one slot".fail) {
    val st = runTrace().states
    assertEquals(chosen(st.values, 1), Set("v"))
  }

  test("two replicas never log different values for the same slot".fail) {
    val st = runTrace().states
    for
        slot <- 0L to 1L
        x    <- names
        y    <- names
        vx   <- st(x).log.get(slot)
        vy   <- st(y).log.get(slot)
    do assertEquals(vx, vy, s"replicas $x and $y disagree on slot $slot")
  }

  test("merging the states of two replicas does not throw".fail) {
    val st = runTrace().states
    Lattice.merge(st("A"), st("B"))
  }

  test("with full-state exchange, only v is chosen and all replicas log v") {
    val st = runTrace(fullState = true).states
    assertEquals(chosen(st.values, 1), Set("v"))
    names.foreach(x => assertEquals(st(x).log.get(1L), Some("v"), s"log of $x"))
  }
}
