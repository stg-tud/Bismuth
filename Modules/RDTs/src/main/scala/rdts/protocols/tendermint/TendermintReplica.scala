// Implementation of the consensus transition logic (Send / OnTimeout / Compute /
// Stabilize) from:
//
//   Francisco Silveira and António Ravara:
//   "Decoupling BFT Consensus via Monotonic Merges and Deterministic Projections"
//
// which itself formalizes Tendermint (Buchman, Kwon, Milosevic, 2018) and its
// TenderTEE variant with trusted hardware (Amoussou-Guenou et al., 2026).
// References to "Sec. n" / "Theorem n" / "Lemma n" in scaladoc refer to the
// paper. Signatures are placeholders only (MockSignature): no real signing is
// performed.
package rdts.protocols.tendermint

import rdts.base.{Bottom, LocalUid, Uid}
import rdts.base.LocalUid.replicaId
import rdts.protocols.tendermint.TendermintState.given
import rdts.protocols.tendermint.Step.*

/** Local, non-replicated registers L and timeout counters C of a replica
  * (Sec. 7: S = ⟨B, C, L⟩). Replicated state lives exclusively in the lattice.
  */
case class LocalState(
    currentHeight: Long = 0,
    currentRound: Long = 0,
    currentStep: Step = Proposal,
    lockedValue: Option[BlockId] = None,
    lockedRound: Long = -1,
    validValue: Option[BlockId] = None,
    validRound: Long = -1,
    proposal: Option[BlockId] = None,
    vote: Option[BlockId] = None,
    decisions: Map[Long, BlockId] = Map.empty,
    // timeout counters C, incremented monotonically
    timeoutProposal: Int = 1,
    timeoutPrevote: Int = 1,
    timeoutPrecommit: Int = 1,
    // TenderTee: next hardware-enforced monotonic counter of this replica
    nextCounter: Long = 0,
):

    def key: (Long, Long, Step) = (currentHeight, currentRound, currentStep)

    def advanceHeight(nextValue: Option[BlockId]): LocalState =
      copy(
        currentHeight = currentHeight + 1,
        currentRound = 0,
        currentStep = Proposal,
        lockedValue = None,
        lockedRound = -1,
        validValue = None,
        validRound = -1,
        proposal = nextValue,
        vote = None,
        timeoutProposal = 1,
        timeoutPrevote = 1,
        timeoutPrecommit = 1,
      )

/** Tendermint / TenderTEE consensus over the TendermintState semilattice,
  * expressed as a pure functional transition system (Sec. 7).
  *
  * Protocol safety is enforced solely through guards operating over read-only
  * queries on the projection π; replicated state is only ever extended by
  * CRDT delta merges of outbound messages (Sec. 8).
  */
case class TendermintReplica(
    state: TendermintState = TendermintState(),
    local: LocalState = LocalState(),
):

    /** Outbound delta for the active step — the message *is* a TendermintState
      * delta. Evidence is attached here, consuming one monotonic counter for
      * TenderTee deployments (the ValidatorSet's trust model decides whether
      * a counter is embedded). The empty state (bottom) encodes "no message".
      */
    def send(using LocalUid, ValidatorSet): TendermintState =
        val vs = summon[ValidatorSet]
        val ev = Evidence(
          ctr = vs.model match
              case TrustModel.Tee       => Some(local.nextCounter)
              case TrustModel.Classical => None
          ,
          sender = replicaId,
          signature = MockSignature(),
        )
        val h = local.currentHeight
        val r = local.currentRound
        local.currentStep match
            case Proposal =>
              if replicaId == leader(h, r)(using vs) && local.proposal.isDefined then
                  TendermintState.proposal(h, r, ProposalMsg(local.proposal.get, local.validRound, ev))
              else Bottom[TendermintState].empty
            case Prevote   => TendermintState.prevote(h, r, Vote(local.proposal, ev))
            case Precommit => TendermintState.precommit(h, r, Vote(local.vote, ev))

    /** Timeout handler: guarantees progress and breaks liveness deadlocks.
      * Timers increment monotonically and shift steps/rounds deterministically.
      */
    def onTimeout: TendermintReplica =
        val l = local.currentStep match
            case Step.Proposal =>
              local.copy(timeoutProposal = local.timeoutProposal + 1, currentStep = Prevote)
            case Prevote =>
              local.copy(timeoutPrevote = local.timeoutPrevote + 1, currentStep = Precommit)
            case Precommit =>
              local.copy(
                timeoutPrecommit = local.timeoutPrecommit + 1,
                currentRound = local.currentRound + 1,
                currentStep = Proposal,
                proposal = local.validValue.orElse(local.proposal),
              )
        TendermintReplica(state, l)

    /** Deterministic guard evaluation over the projected state (Sec. 7).
      * Returns a new local state; the replicated state is never modified here.
      */
    def compute()(using ValidatorSet): TendermintReplica =
        val h = local.currentHeight
        val r = local.currentRound

        local.currentStep match
            case Proposal =>
              state.getProposal(h, r) match
                  case Some(p) if isValid(p.block) =>
                    val justified =
                      state.getPrevoteQuorum(h, p.validRound).contains(p.block) &&
                      p.validRound >= local.lockedRound &&
                      p.validRound < r
                    val unlocked =
                      local.lockedRound == -1 || local.lockedValue.contains(p.block)
                    if justified || unlocked then
                        TendermintReplica(state, local.copy(proposal = Some(p.block), currentStep = Prevote))
                    else TendermintReplica(state, local.copy(currentStep = Prevote))
                  case Some(_) =>
                    // invalid proposal: prevote nil
                    TendermintReplica(state, local.copy(proposal = None, currentStep = Prevote))
                  case None => this // fixed point

            case Prevote =>
              state.getPrevoteQuorum(h, r) match
                  case Some(b) if isValid(b) && state.getProposal(h, r).exists(_.block == b) =>
                    // lock b and precommit it (Lemma 6 anchor, Line 39)
                    TendermintReplica(
                      state,
                      local.copy(
                        lockedValue = Some(b),
                        lockedRound = r,
                        validValue = Some(b),
                        validRound = r,
                        vote = Some(b),
                        currentStep = Precommit,
                      )
                    )
                  case _ =>
                    val nil = if state.hasNilQuorum(h, r, Prevote) then None else local.proposal
                    TendermintReplica(state, local.copy(vote = nil, currentStep = Precommit))

            case Precommit =>
              val bv = state.getPrevoteQuorum(h, r)
              val lv = bv match
                  case Some(b) if isValid(b) && state.getProposal(h, r).exists(_.block == b) =>
                    local.copy(validValue = Some(b), validRound = r)
                  case _ => local
              state.getPrecommitQuorum(lv.currentHeight, lv.currentRound) match
                  case Some(b) if isValid(b) && !lv.decisions.contains(lv.currentHeight) =>
                    // decision register is write-once per height (Theorem 6)
                    val next = nextProposalValue(lv)
                    TendermintReplica(
                      state,
                      lv.copy(decisions = lv.decisions + (lv.currentHeight -> b))
                        .advanceHeight(next)
                    )
                  case _ =>
                    if state.hasNilQuorum(lv.currentHeight, lv.currentRound, Precommit) then
                        TendermintReplica(
                          state,
                          lv.copy(
                            currentRound = lv.currentRound + 1,
                            currentStep = Proposal,
                            proposal = lv.validValue.orElse(local.proposal),
                          )
                        )
                    else this // fixed point: no guard satisfied

    /** Fixed-point stabilization (Sec. 8): repeatedly applies Compute until
      * key(S) = key(S1). Terminates over a finite lattice snapshot (Theorem 7).
      */
    def stabilize(using ValidatorSet): TendermintReplica =
        val s1 = compute()
        if s1.local.key == local.key then s1
        else s1.stabilize

    // -- helpers ---------------------------------------------------------------

    /** Round-robin leader designation for (h, r). */
    def leader(h: Long, r: Long)(using vs: ValidatorSet): Uid =
        val members = vs.members.toIndexedSeq.sorted
        members(((h + r) % members.size).toInt)

    def isValid(b: BlockId): Boolean = true // application-level block validity

    private def nextProposalValue(l: LocalState): Option[BlockId] =
      l.validValue.orElse(Some(BlockId(l.currentHeight)))
