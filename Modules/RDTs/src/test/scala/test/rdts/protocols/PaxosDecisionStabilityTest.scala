package test.rdts.protocols

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.protocols.{Participants, Paxos}
import rdts.protocols.Util.Agreement

import scala.collection.mutable

/** Stability of the single-decree Paxos decision: once a replica has decided, its decision
  * must not change.
  *
  * Regression test. `phase1a(value)` stores the value as a vote in the round `BallotNum(me, -1)`.
  * A second `phase1a(value)` (which `Consensus.propose` issues on every retry) used to add a
  * second vote of the same replica, so that round became `Invalid`. `Paxos.decision` returned the
  * first non-`Undecided` round in `Map` iteration order, so it could return `Invalid` instead of
  * the real decision.
  *
  * Now `phase1a(value)` stores the value only once, and `decision` returns the decided round that
  * is lowest in ballot order, whatever the other rounds contain.
  */
class PaxosDecisionStabilityTest extends munit.FunSuite {

  type S = Paxos[Boolean]
  val names: List[String]      = List("A", "B", "C")
  given Participants           = Participants(names.map(Uid.predefined).toSet)
  def lid(n: String): ReplicaId = ReplicaId.predefined(n)

  class Run {
    val st: mutable.Map[String, S] = mutable.LinkedHashMap.from(names.map(_ -> Paxos[Boolean]()))
    def merge(x: String, y: String): Unit = st(x) = Lattice.merge(st(x), st(y))
    def doOp(x: String)(op: ReplicaId ?=> S => S): Unit =
      given ReplicaId = lid(x)
      st(x) = Lattice.merge(st(x), op(st(x)))
  }

  /** Brings all three replicas to a decision on `false`, with A having proposed `true`. */
  def decided(): Run = {
    val r = Run()
    import r.*
    doOp("A")(_.phase1a(true))
    merge("B", "A"); doOp("B")(_.phase1a(false))
    merge("C", "B"); doOp("C")(_.phase1b)
    merge("B", "C"); doOp("B")(_.phase2a)
    merge("C", "B"); doOp("C")(_.phase2b)
    merge("A", "C")
    r
  }

  test("trace precondition: A has decided false") {
    assertEquals(decided().st("A").decision, Agreement.Decided(false))
  }

  test("a decided replica stays decided after calling phase1a(value) again") {
    val r = decided()
    import r.*
    doOp("A")(_.phase1a)
    doOp("A")(_.phase1a(false))
    assertEquals(st("A").decision, Agreement.Decided(false))
  }

  test("merging another replica's state does not change a decision") {
    val r = decided()
    import r.*
    doOp("A")(_.phase1a)
    doOp("A")(_.phase1a(false))
    merge("C", "A")
    assertEquals(st("C").decision, Agreement.Decided(false))
  }
}
