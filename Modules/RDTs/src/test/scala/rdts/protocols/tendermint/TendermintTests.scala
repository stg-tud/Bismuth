package rdts.protocols.tendermint

import rdts.base.Uid
import rdts.base.Lattice.syntax
import rdts.protocols.tendermint.TendermintState.given

class TendermintTests extends munit.FunSuite:

    val members = (0 until 4).map(i => Uid(s"v$i")).toSet
    val vs      = ValidatorSet(members, TrustModel.Classical)
    val vs4     = vs

    private def signed(uid: String): Evidence = Evidence(None, Uid(uid), MockSignature())
    private def tee(uid: String, ctr: Long)   = Evidence(Some(ctr), Uid(uid), MockSignature())

    /** Proposal by the round-robin leader of (h, r) plus a prevote quorum for b. */
    private def quorumRound(h: Long, r: Long, b: BlockId): TendermintState =
        val leader   = s"v${(h + r) % 4}"
        val proposal = TendermintState.proposal(h, r, ProposalMsg(b, -1, signed(leader)))
        val prevotes = (0 until 3).map(i => TendermintState.prevote(h, r, Vote(Some(b), signed(s"v$i"))))
        (proposal +: prevotes).reduce((a, b) => a.merge(b))

    private def quorumPrecommits(h: Long, r: Long, b: BlockId): TendermintState =
        (0 until 3).map(i => TendermintState.precommit(h, r, Vote(Some(b), signed(s"v$i")))).reduce((a, b) => a.merge(b))

    test("CRDT convergence is independent of delivery order (Lemma 2)"):
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

    // ---------------------------------------------------------------------
    // Theorem 4 (Lock Safety): if a prevote quorum certificate for block b is
    // formed in round r, then no conflicting prevote quorum for b' != b can
    // be formed in any subsequent round r' > r.
    // ---------------------------------------------------------------------
    test("Theorem 4: a locked replica never prevotes a conflicting block, so no conflicting quorum forms"):
        given ValidatorSet = vs4
        val b1 = BlockId(1)
        val b2 = BlockId(2)

        // round 0: replica locks b1 via a prevote quorum
        val locked = TendermintReplica(state = quorumRound(0, 0, b1)).stabilize
        assertEquals(locked.local.lockedValue, Some(b1))
        assertEquals(locked.local.lockedRound, 0L)

        // advance to round 1 (timeout), then a Byzantine leader proposes b2
        // with validRound 0 -- no quorum certificate for b2 at a higher round
        val atRound1 = locked.onTimeout
        val state1   = atRound1.state.merge(
            TendermintState.proposal(0, 1, ProposalMsg(b2, 0, signed("v1"))) // leader of (0,1) is v1
        )
        val r2 = TendermintReplica(state1, atRound1.local).stabilize

        // the replica must not prevote b2 (it holds its locked value instead,
        // paper Line 33 fallback), and the lock on b1 is preserved
        assert(r2.local.vote != Some(b2))
        assertEquals(r2.local.lockedValue, Some(b1))
        assertEquals(r2.local.lockedRound, 0L)

        // even with Byzantine equivocation (2 conflicting prevotes injected),
        // a conflicting quorum at round 1 needs the locked replica's vote
        val conflictingVotes = (0 until 2)
            .map(i => TendermintState.prevote(0, 1, Vote(Some(b2), signed(s"v$i"))))
            .fold(state1)((a, d) => a.merge(d))
        assertEquals(conflictingVotes.getPrevoteQuorum(0, 1), None)

    test("Theorem 4 (converse): a proposal justified by a quorum at a higher validRound unlocks"):
        given ValidatorSet = vs4
        val b1 = BlockId(1)
        val b2 = BlockId(2)

        val locked = TendermintReplica(state = quorumRound(0, 0, b1)).stabilize
        // round 1: prevote quorum forms for b2 (no proposal adoption possible
        // yet at the same round), replica times out into round 2
        val atRound1 = locked.onTimeout
            .copy(state = locked.state.merge(
                (0 until 3).map(i => TendermintState.prevote(0, 1, Vote(Some(b2), signed(s"v$i")))).reduce((a, b) => a.merge(b))
            ))
        val atRound2 = atRound1.onTimeout.onTimeout.onTimeout // Proposal → Prevote → Precommit → round 2 Proposal

        // round 2: leader proposes b2 with validRound 1 > lockedRound 0,
        // backed by the round-1 prevote quorum (guard of paper Line 27)
        val state2 = atRound2.state
            .merge(TendermintState.proposal(0, 2, ProposalMsg(b2, 1, signed("v2")))) // leader of (0,2) is v2
            .merge(
                (0 until 3).map(i => TendermintState.prevote(0, 2, Vote(Some(b2), signed(s"v$i")))).reduce((a, b) => a.merge(b))
            )
        val r2 = TendermintReplica(state2, atRound2.local).stabilize

        // the lock was legitimately updated onto b2
        assertEquals(r2.local.lockedValue, Some(b2))
        assertEquals(r2.local.lockedRound, 2L)

    // ---------------------------------------------------------------------
    // Theorem 5 (Agreement): no two correct replicas can decide conflicting
    // values for the same height.
    // ---------------------------------------------------------------------
    test("Theorem 5: replicas with differently-ordered delivery decide the same block"):
        given ValidatorSet = vs4
        val b1 = BlockId(1)
        val base = quorumRound(0, 0, b1).merge(quorumPrecommits(0, 0, b1))

        // simulate two replicas whose transport delivered the same deltas in
        // different orders (and with duplication)
        val deltas = Seq(base, quorumRound(0, 0, b1), quorumPrecommits(0, 0, b1))
        val ra = TendermintReplica(state = deltas.reduce((a, b) => a.merge(b))).stabilize
        val rb = TendermintReplica(state = deltas.reverse.reduce((a, b) => a.merge(b))).stabilize

        assertEquals(ra.local.decisions.get(0), Some(b1))
        assertEquals(rb.local.decisions.get(0), Some(b1))

    // ---------------------------------------------------------------------
    // Theorem 6 (Decision Stability): if a correct replica reaches
    // decisions[h] = b, the register is immutable in all subsequent steps.
    // ---------------------------------------------------------------------
    test("Theorem 6: the decision register is write-once per height"):
        given ValidatorSet = vs4
        val b1 = BlockId(1)
        val b2 = BlockId(2)

        // replica decides b1 at height 0 but stays at round 1 of height 0
        val committed = TendermintReplica(
            state = quorumRound(0, 0, b1).merge(quorumPrecommits(0, 0, b1))
        ).stabilize
        assertEquals(committed.local.decisions.get(0), Some(b1))

        // adversarially inject a full conflicting certificate for b2 at round 1
        // (only possible via Byzantine behavior; the CRDT layer accepts it)
        val injected = committed.state
            .merge(quorumRound(0, 1, b2))
            .merge(quorumPrecommits(0, 1, b2))
        // rewind to round 1 of height 0 while keeping the decision register
        val atRound1 = committed.local.copy(
            currentRound = 1,
            currentStep = Step.Proposal,
            proposal = Some(b2),
            lockedValue = Some(b2),
            lockedRound = 1,
        )
        val r2 = TendermintReplica(injected, atRound1).stabilize

        // Line 48's guard (decisions[h] == empty) evaluates false: the
        // decision for b1 is never overwritten by b2
        assertEquals(r2.local.decisions.get(0), Some(b1))

    // ---------------------------------------------------------------------
    // Corollary 1 (Asynchronous Decoupling / Transport Invariance): consensus
    // safety is invariant to message duplication, reordering, and scheduling.
    // ---------------------------------------------------------------------
    test("Corollary 1: duplicated and reordered deltas yield identical replica outcomes"):
        given ValidatorSet = vs4
        val b1 = BlockId(1)

        val deltas = Seq(
            quorumRound(0, 0, b1),
            quorumPrecommits(0, 0, b1),
        )
        // same multiset, delivered in three different schedulings, with duplicates
        val schedulings = Seq(
            deltas ++ deltas,
            deltas.reverse ++ deltas,
            deltas.tail ++ deltas ++ deltas,
        )

        val outcomes = schedulings.map { msgs =>
            val replica = TendermintReplica(state = msgs.reduce((a, b) => a.merge(b))).stabilize
            (replica.local.decisions.get(0), replica.local.lockedValue, replica.local.lockedRound)
        }

        assertEquals(outcomes(0), outcomes(1))
        assertEquals(outcomes(1), outcomes(2))
        assertEquals(outcomes.head._1, Some(b1))
