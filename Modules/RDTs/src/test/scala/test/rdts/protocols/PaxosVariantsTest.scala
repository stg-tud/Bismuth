package test.rdts.protocols

import rdts.base.{Lattice, Uid}
import rdts.datatypes.Epoch
import rdts.protocols.paxosVariants.{ConfigurationRound, EpochPaxos, GenPaxos, ReconfigurablePaxos}
import rdts.protocols.{BallotNum, Participants, Paxos, PaxosRound, Vote, Voting}
import rdts.protocols.Util.Agreement

/** Tests for the Paxos variants in `rdts.protocols.paxosVariants`. */
class PaxosVariantsTest extends munit.FunSuite {

  val a: Uid = Uid.predefined("a")
  val b: Uid = Uid.predefined("b")
  val c: Uid = Uid.predefined("c")
  given Participants = Participants(Set(a, b, c))

  /** A Paxos instance whose decision is `Decided(v)`. */
  def decidedPaxos[A](v: A): Paxos[A] =
    Paxos(Map(BallotNum(a, 0) -> PaxosRound(proposals = Voting(Set(Vote(a, v), Vote(b, v))))))

  /** A Paxos instance whose decision is `Invalid` (replica a voted twice in one round). */
  def invalidPaxos[A](v1: A, v2: A): Paxos[A] =
    Paxos(Map(BallotNum(a, 0) -> PaxosRound(proposals = Voting(Set(Vote(a, v1), Vote(a, v2))))))

  // `Paxos()` in `nextDecision` is inferred as `Paxos[Nothing]` and the case classes are
  // invariant, so the result of nextDecision cannot be merged into the state it came from.
  test("nextDecision has the type of its state") {
    assertNoDiff(compileErrors("(EpochPaxos[Boolean]().nextDecision: EpochPaxos[Boolean])"), "")
    assertNoDiff(compileErrors("(GenPaxos[Boolean]().nextDecision(Set.empty): GenPaxos[Boolean])"), "")
    assertNoDiff(compileErrors("(ReconfigurablePaxos[Boolean]().nextDecision: ReconfigurablePaxos[Boolean])"), "")
  }

  // The tests below cast the result of nextDecision to work around the type above
  // (the cast is erased, the runtime values are unaffected).

  test("GenPaxos.nextDecision with a predecessor that is not known locally does not throw") {
    val s = GenPaxos[Boolean]()
    val d = s.nextDecision(Set(Uid.predefined("unknown"))).asInstanceOf[GenPaxos[Boolean]]
    assertEquals(Lattice.merge(s, d), s, "an unknown predecessor is not decided, so no operation is created")
  }

  test("ReconfigurablePaxos.nextDecision does not throw when nextMembers is Invalid") {
    val nextMembers = invalidPaxos(Set(a), Set(b))
    assertEquals(nextMembers.decision, Agreement.Invalid)
    val s = ReconfigurablePaxos[Boolean](Epoch(0, ConfigurationRound(Set(a, b, c), nextMembers, decidedPaxos(true))))
    val d = s.nextDecision.asInstanceOf[ReconfigurablePaxos[Boolean]]
    assertEquals(Lattice.merge(s, d), s)
  }
}
