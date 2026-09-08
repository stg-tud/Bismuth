package rdts.protocols.tendermint

import rdts.base.{LocalUid, Uid}

class BFTSmokeTest extends munit.FunSuite:

    test("CRDT convergence is independent of delivery order (Lemma 2)"):
        given ValidatorSet = ValidatorSet((0 until 4).map(i => Uid(s"v$i")).toSet, TrustModel.Classical)
        given LocalUid     = LocalUid(Uid("v0"))
        val mA             = InMsg.Prevote(0, 0, Vote(Some(BlockId(1)), Signed(MockSignature())))
        val mB             = InMsg.Prevote(0, 0, Vote(Some(BlockId(2)), Signed(MockSignature())))
        val x              = BlockchainState[Signed]().merge(mA).merge(mB)
        val y              = BlockchainState[Signed]().merge(mB).merge(mA)
        // reordering and duplication must not matter
        assertEquals(x, y)
        assertEquals(x.merge(mA), x)

    test("prevote quorum forms at 2f+1; equivocation nullifies the equivocator (Thm. 3, Def. 5)"):
        given ValidatorSet = ValidatorSet((0 until 4).map(i => Uid(s"v$i")).toSet, TrustModel.Classical)
        var b              = BlockchainState[Signed]()
        for i <- 0 until 3 do
            given LocalUid = LocalUid(Uid(s"v$i"))
            b = b.merge(InMsg.Prevote(0, 0, Vote(Some(BlockId(1)), Signed(MockSignature()))))
        assertEquals(b.getPrevoteQuorum(0, 0), Some(BlockId(1)))

        // v0 equivocates: its projection becomes bottom, only 2 canonical votes remain
        given LocalUid = LocalUid(Uid("v0"))
        val b4         = b.merge(InMsg.Prevote(0, 0, Vote(Some(BlockId(2)), Signed(MockSignature()))))
        assertEquals(b4.getPrevoteQuorum(0, 0), None)

        // only 2 votes: no quorum
        var b5 = BlockchainState[Signed]()
        for i <- 0 until 2 do
            given LocalUid = LocalUid(Uid(s"v$i"))
            b5 = b5.merge(InMsg.Prevote(0, 0, Vote(Some(BlockId(1)), Signed(MockSignature()))))
        assertEquals(b5.getPrevoteQuorum(0, 0), None)

    test("TenderTEE projection selects minimal counter and quorum is f+1 (Lemma 4, 5)"):
        val teeMembers     = (0 until 3).map(i => Uid(s"v$i")).toSet
        given ValidatorSet = ValidatorSet(teeMembers, TrustModel.Tee)

        def add(
            b: BlockchainState[TeeSigned],
            uid: String,
            counter: Long,
            block: Option[BlockId]
        ): BlockchainState[TeeSigned] =
            given LocalUid = LocalUid(Uid(uid))
            b.merge(InMsg.Precommit(0, 0, Vote(block, TeeSigned(counter, MockSignature()))))

        // two validators vote, with out-of-order delivery of their counters
        val bt = add(
          add(add(BlockchainState(), "v0", 2, Some(BlockId(5))), "v0", 0, Some(BlockId(5))),
          "v1",
          1,
          Some(BlockId(5))
        )
        // f+1 = 2 canonical votes suffice; entries carry duplicates
        assertEquals(bt.getPrecommitQuorum(0, 0), Some(BlockId(5)))

        // nil quorum
        val bn = add(add(BlockchainState(), "v0", 0, None), "v1", 1, None)
        assert(bn.hasNilQuorum(0, 0, Step.Precommit))

    test("compute drives a replica from Proposal to Precommit on quorum"):
        given vs4: ValidatorSet = ValidatorSet((0 until 4).map(i => Uid(s"v$i")).toSet, TrustModel.Classical)
        var b                   = BlockchainState[Signed]()
        // leader for (0,0) proposes block 1
        locally {
          given LocalUid = LocalUid(Uid(s"v${(0 + 0) % 4}"))
          b = b.merge(InMsg.Proposal(0, 0, ProposalMsg(BlockId(1), -1, Signed(MockSignature()))))
          // 3 prevotes for block 1
          for i <- 0 until 3 do
              given LocalUid = LocalUid(Uid(s"v$i"))
              b = b.merge(InMsg.Prevote(0, 0, Vote(Some(BlockId(1)), Signed(MockSignature()))))
        }
        locally {
          given LocalUid = LocalUid(Uid("v0"))
          val t          = Tendermint(state = b).stabilize
          assertEquals(t.local.currentStep, Step.Precommit)
          assertEquals(t.local.lockedValue, Some(BlockId(1)))
          assertEquals(t.local.lockedRound, 0L)
        }
