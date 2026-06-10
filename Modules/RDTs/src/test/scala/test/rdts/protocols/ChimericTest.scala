package test.rdts.protocols

import rdts.base.{LocalUid, Uid}
import rdts.protocols.{Consensus, Participants}
import rdts.protocols.chimeric.{Chimeric, QuorumConfig}
import rdts.protocols.Util.Agreement.*

class ChimericTest extends munit.FunSuite {

  val u1 = Uid("node1")
  val u2 = Uid("node2")
  val u3 = Uid("node3")

  val id1: LocalUid = LocalUid(u1)
  val id2: LocalUid = LocalUid(u2)
  val id3: LocalUid = LocalUid(u3)

  given Participants = Participants(Set(u1, u2, u3))

  private def manualProposalState(
      votes: List[(Uid, Int)]
  ): Chimeric[Int] = {
    val proposalVoting =
      votes.foldLeft(rdts.protocols.Voting[Int]()) {
        case (acc, (uid, value)) =>
          given LocalUid = LocalUid(uid)
          acc.voteFor(value)
      }

    Chimeric(
      rounds = Map(
        rdts.protocols.BallotNum(Uid("manual"), 0) ->
        rdts.protocols.PaxosRound(
          proposals = proposalVoting
        )
      )
    )
  }

  private def runSingleProposal(
      cfg: QuorumConfig,
      value: Int,
      proposer: LocalUid = id1
  ): Chimeric[Int] = {
    given QuorumConfig = cfg

    var c: Chimeric[Int] = Consensus[Chimeric].empty[Int]

    c = c `merge` c.propose(value)(using proposer)

    for _ <- 1 to 5 do
        c = c `merge` c.upkeep()(using id1)
        c = c `merge` c.upkeep()(using id2)
        c = c `merge` c.upkeep()(using id3)

    c
  }

  private def runTwoProposals(
      cfg: QuorumConfig,
      v1: Int,
      v2: Int
  ): Chimeric[Int] = {
    given QuorumConfig = cfg

    var c: Chimeric[Int] = Consensus[Chimeric].empty[Int]

    c = c `merge` c.propose(v1)(using id1)
    c = c `merge` c.propose(v2)(using id2)

    for _ <- 1 to 5 do
        c = c `merge` c.upkeep()(using id1)
        c = c `merge` c.upkeep()(using id2)
        c = c `merge` c.upkeep()(using id3)

    c
  }

  private def runThreeProposals(
      cfg: QuorumConfig,
      v1: Int,
      v2: Int,
      v3: Int
  ): Chimeric[Int] = {
    given QuorumConfig = cfg

    var c: Chimeric[Int] = Consensus[Chimeric].empty[Int]

    c = c `merge` c.propose(v1)(using id1)
    c = c `merge` c.propose(v2)(using id2)
    c = c `merge` c.propose(v3)(using id3)

    for _ <- 1 to 5 do
        c = c `merge` c.upkeep()(using id1)
        c = c `merge` c.upkeep()(using id2)
        c = c `merge` c.upkeep()(using id3)

    c
  }

  test("full trust 3-of-3 reaches decision") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1, u2, u3)),
      u2 -> Set(Set(u1, u2, u3)),
      u3 -> Set(Set(u1, u2, u3))
    )

    val c = runSingleProposal(summon[QuorumConfig], 42)

    assertEquals(c.result, Some(42))
    assertEquals(c.decision, Decided(42))
  }

  test("2-of-3 style slices also reach decision") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1, u2), Set(u1, u3)),
      u2 -> Set(Set(u2, u1), Set(u2, u3)),
      u3 -> Set(Set(u3, u1), Set(u3, u2))
    )

    val c = runSingleProposal(summon[QuorumConfig], 55)

    assertEquals(c.result, Some(55))
    assertEquals(c.decision, Decided(55))
  }

  test("disconnected trust can still decide if a deciding voter set forms") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1, u2)),
      u2 -> Set(Set(u2, u1)),
      u3 -> Set(Set(u3))
    )

    val c = runSingleProposal(summon[QuorumConfig], 99)

    assertEquals(c.result, Some(99))
    assertEquals(c.decision, Decided(99))
  }

  test("self-only trust can still decide under local quorum semantics") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1)),
      u2 -> Set(Set(u2)),
      u3 -> Set(Set(u3))
    )

    val c = runThreeProposals(summon[QuorumConfig], 7, 8, 9)

    assertEquals(c.result, Some(9))
    assertEquals(c.decision, Decided(9))
  }

  test(
    "self-only trust cannot decide under when they only belive themselves and stick to their votes local quorum semantics"
  ) {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1)),
      u2 -> Set(Set(u2)),
      u3 -> Set(Set(u3))
    )

    val c = manualProposalState(List((u1, 7), (u2, 8), (u3, 9)))

    assertEquals(c.result, None)
    assertEquals(c.decision, Undecided)
  }

  test("multiple proposers under full trust decides one value") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1, u2, u3)),
      u2 -> Set(Set(u1, u2, u3)),
      u3 -> Set(Set(u1, u2, u3))
    )

    val c = runTwoProposals(summon[QuorumConfig], 42, 2)

    assertEquals(c.result, Some(2))
    assertEquals(c.decision, Decided(2))
  }

  test("result and decision are consistent under reachable quorum") {
    given QuorumConfig = Map(
      u1 -> Set(Set(u1, u2), Set(u1, u3)),
      u2 -> Set(Set(u2, u1), Set(u2, u3)),
      u3 -> Set(Set(u3, u1), Set(u3, u2))
    )

    val c = runSingleProposal(summon[QuorumConfig], 100)

    assertEquals(c.result, Some(100))
    assertEquals(c.decision, Decided(100))
  }
}
