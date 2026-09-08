package rdts.protocols.tendermint

import rdts.base.Uid
import rdts.base.Lattice.syntax
import rdts.protocols.tendermint.TendermintState.given

class BFTSmokeTest extends munit.FunSuite:

    val members = (0 until 4).map(i => Uid(s"v$i")).toSet
    val vs      = ValidatorSet(members, TrustModel.Classical)

    private def signed(uid: String): Evidence = Evidence(None, Uid(uid), MockSignature())
    private def tee(uid: String, ctr: Long)   = Evidence(Some(ctr), Uid(uid), MockSignature())

    test("CRDT convergence is independent of delivery order (Lemma 2)"):
        given ValidatorSet = vs
        val mA             = TendermintState.prevote(0, 0, Vote(Some(BlockId(1)), signed("v0")))
        val mB             = TendermintState.prevote(0, 0, Vote(Some(BlockId(2)), signed("v1")))
        val x              = TendermintState().merge(mA).merge(mB)
        val y              = TendermintState().merge(mB).merge(mA)
        // reordering and duplication must not matter
        assertEquals(x, y)
        assertEquals(x.merge(mA), x)

    test("prevote quorum forms at 2f+1; equivocation nullifies the equivocator (Thm. 3, Def. 5)"):
        given ValidatorSet = vs
        var b              = TendermintState()
        for i <- 0 until 3 do
            b = b.merge(TendermintState.prevote(0, 0, Vote(Some(BlockId(1)), signed(s"v$i"))))
        assertEquals(b.getPrevoteQuorum(0, 0), Some(BlockId(1)))

        // v0 equivocates: its projection becomes bottom, only 2 canonical votes remain
        val b4 = b.merge(TendermintState.prevote(0, 0, Vote(Some(BlockId(2)), signed("v0"))))
        assertEquals(b4.getPrevoteQuorum(0, 0), None)

        // only 2 votes: no quorum
        var b5 = TendermintState()
        for i <- 0 until 2 do
            b5 = b5.merge(TendermintState.prevote(0, 0, Vote(Some(BlockId(1)), signed(s"v$i"))))
        assertEquals(b5.getPrevoteQuorum(0, 0), None)

    test("TenderTEE projection selects minimal counter and quorum is f+1 (Lemma 4, 5)"):
        val teeMembers     = (0 until 3).map(i => Uid(s"v$i")).toSet
        given ValidatorSet = ValidatorSet(teeMembers, TrustModel.Tee)

        def add(b: TendermintState, uid: String, counter: Long, block: Option[BlockId]): TendermintState =
          b.merge(TendermintState.precommit(0, 0, Vote(block, tee(uid, counter))))

        // two validators vote, with out-of-order delivery of their counters
        val bt = add(
          add(add(TendermintState(), "v0", 2, Some(BlockId(5))), "v0", 0, Some(BlockId(5))),
          "v1",
          1,
          Some(BlockId(5))
        )
        // f+1 = 2 canonical votes suffice; entries carry duplicates
        assertEquals(bt.getPrecommitQuorum(0, 0), Some(BlockId(5)))

        // nil quorum
        val bn = add(add(TendermintState(), "v0", 0, None), "v1", 1, None)
        assert(bn.hasNilQuorum(0, 0, Step.Precommit))

    test("compute drives a replica from Proposal to Precommit on quorum"):
        val vs4 = ValidatorSet((0 until 4).map(i => Uid(s"v$i")).toSet, TrustModel.Classical)
        var b   = TendermintState()
        // leader for (0,0) proposes block 1
        locally {
          given ValidatorSet = vs4
          b = b.merge(TendermintState.proposal(0, 0, ProposalMsg(BlockId(1), -1, signed("v0"))))
          // 3 prevotes for block 1
          for i <- 0 until 3 do
              b = b.merge(TendermintState.prevote(0, 0, Vote(Some(BlockId(1)), signed(s"v$i"))))
        }
        locally {
          given ValidatorSet = vs4
          val t              = TendermintReplica(state = b).stabilize
          assertEquals(t.local.currentStep, Step.Precommit)
          assertEquals(t.local.lockedValue, Some(BlockId(1)))
          assertEquals(t.local.lockedRound, 0L)
        }
