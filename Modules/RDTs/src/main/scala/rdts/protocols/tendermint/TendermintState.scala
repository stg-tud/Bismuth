// Implementation of the BFT consensus framework from:
//
//   Francisco Silveira and António Ravara:
//   "Decoupling BFT Consensus via Monotonic Merges and Deterministic Projections"
//   (Nova School of Science and Technology).
//
// The paper instantiates its abstraction on Tendermint
// (Buchman, Kwon, Milosevic: "The Latest Gossip on BFT Consensus", 2018)
// and TenderTEE (Amoussou-Guenou, Beltrando, Herlihy, Potop-Butucaru:
// "Byzantine Reliable Broadcast and Tendermint Consensus with Trusted Components",
// Theoretical Computer Science, 2026).
//
// Scaladoc references to "Lemma n", "Def. n", "Thm. n" and "Sec. n" refer to
// that paper. This implementation follows the paper's design: replicated state
// is a hierarchical join-semilattice CRDT, protocol safety rests on
// deterministic projections π of that lattice, and the consensus logic is a
// pure functional transition system.
package rdts.protocols.tendermint

import rdts.base.{Bottom, Lattice, Uid}
import rdts.protocols.tendermint.Step.*

/** Execution phase within a round. */
enum Step:
    case Proposal, Prevote, Precommit

/** Trust configuration governing adversarial capabilities.
  *
  *   - Classical: digital signatures, equivocation possible, quorum 2f + 1 (n >= 3f + 1)
  *   - Tee:       hardware-enforced monotonic counters, no equivocation, quorum f + 1
  *
  * The trust model is a property of the validator set, not of the evidence
  * type: the same Evidence shape is used in both deployments.
  */
enum TrustModel:
    case Classical, Tee

case class BlockId(id: Long)

/** Placeholder signature. No real signing or verification happens anywhere in
  * this implementation: signatures exist purely to model the shape of the
  * cryptographic binding required by the paper. A real deployment would
  * substitute an authenticated evidence type (e.g. Ed25519 signatures, or BLS
  * aggregate signatures) here; the protocol logic itself never inspects the
  * signature contents.
  */
case class MockSignature()

/** Cryptographic binding attached to every protocol message.
  *
  * Carries the authenticated sender identity of the message (in a real
  * deployment recovered by verifying the signature) and, in the TenderTee
  * deployment, a hardware-enforced monotonic counter (Def. 3) — `ctr` is
  * `None` in the classical Tendermint deployment. Whether a counter is
  * present/checked is decided by the ValidatorSet's TrustModel. No real
  * cryptographic verification is performed — see [[MockSignature]].
  */
case class Evidence(ctr: Option[Long], sender: Uid, signature: MockSignature)

/** Validator set together with the trust model determining quorum thresholds. */
case class ValidatorSet(members: Set[Uid], model: TrustModel):
    def f: Int = model match
        case TrustModel.Classical => (members.size - 1) / 3 // n >= 3f + 1
        case TrustModel.Tee       => (members.size - 1) / 2 // n >= 2f + 1
    def quorum: Int = model match
        case TrustModel.Classical => 2 * f + 1
        case TrustModel.Tee       => f + 1

/** A vote (pre-vote or pre-commit) for a block or nil, bound to evidence. */
case class Vote(block: Option[BlockId], evidence: Evidence)

/** A proposal carrying the validRound of the proposer's lock. */
case class ProposalMsg(block: BlockId, validRound: Long, evidence: Evidence)

/** CRDT replicated state for a single round (Sec. 4). */
case class RoundState(
    proposals: Set[ProposalMsg] = Set.empty[ProposalMsg],
    preVotes: Map[Uid, Set[Vote]] = Map.empty[Uid, Set[Vote]],
    preCommits: Map[Uid, Set[Vote]] = Map.empty[Uid, Set[Vote]],
)

case class HeightState(rounds: Map[Long, RoundState] = Map.empty[Long, RoundState])

/** Hierarchical join-semilattice over (height, round, step, validator) coordinates
  * (Lemma 1). Merge is key-wise map merge with set union on overlap, hence
  * associative, commutative and idempotent: replicas that incorporate the same
  * set of deltas converge (Lemma 2), independent of delivery order and duplication.
  */
case class TendermintState(heights: Map[Long, HeightState] = Map.empty[Long, HeightState]) {

  // -- Deterministic state projections (Sec. 5) ----------------------------

  def roundState(h: Long, r: Long): RoundState =
    heights.get(h).flatMap(_.rounds.get(r)).getOrElse(RoundState())

  /** Deterministic projection π over a set of observed entries (Lemma 3):
    *
    *   - Classical: canonical entry iff the set is a singleton, ⊥ on any conflict
    *   - Tee:       unique minimal-counter entry (Lemma 4), ⊥ if empty
    *
    * `ev` extracts the evidence carried by an entry.
    */
  def project[V](entries: Set[V])(ev: V => Evidence)(using vs: ValidatorSet): Option[V] =
    vs.model match
        case TrustModel.Classical =>
          if entries.size == 1 then entries.headOption else None
        case TrustModel.Tee =>
          entries
            .map(e => (e, ev(e).ctr))
            .collect { case (e, Some(ctr)) => (e, ctr) }
            .minByOption(_._2)
            .map(_._1)

  /** TenderTee per-(validator, step) contiguous-prefix admission rule (Def. 3):
    * a message is admitted only if it extends the next expected counter.
    */
  def canAdmit(existing: Set[Vote], vote: Vote)(using vs: ValidatorSet): Boolean =
    (vote.evidence.ctr, vs.model) match
        case (Some(counter), TrustModel.Tee) =>
          val counters = existing.collect { case Vote(_, Evidence(Some(c), _, _)) => c }
          counters.forall(_ < counter) && counter == counters.maxOption.getOrElse(-1L) + 1
        case (None, TrustModel.Classical) => true
        case _                            => false

  // -- Deterministic protocol queries (Sec. 6) ------------------------------

  /** Def. 4: block proposed by the designated leader for (h, r).
    * Proposals are not keyed by validator, so π is applied over the whole
    * proposal slot: a singleton yields the proposal, any conflict yields ⊥
    * (Classical) or the minimal-counter entry (Tee).
    */
  def getProposal(h: Long, r: Long)(using vs: ValidatorSet): Option[ProposalMsg] =
    project(roundState(h, r).proposals)(_.evidence)

  private def votesIn(
      h: Long,
      r: Long,
      select: RoundState => Map[Uid, Set[Vote]],
  )(using vs: ValidatorSet): Map[Uid, Option[Vote]] =
    select(roundState(h, r)).view.mapValues(entries => project(entries)(_.evidence)).toMap

  private def quorumBlock(
      h: Long,
      r: Long,
      select: RoundState => Map[Uid, Set[Vote]],
  )(using vs: ValidatorSet): Option[BlockId] =
      val counts = votesIn(h, r, select).values.flatten
        .filter(_.block.isDefined)
        .groupBy(_.block)
        .map((b, vts) => (b, vts.size))
      val winners = counts.filter((_, c) => c >= vs.quorum).keys
      // quorum validity (Thm. 3) implies at most one such block
      if winners.size == 1 then winners.head else None

  /** Def. 5: unique block with a prevote-threshold certificate at (h, r). */
  def getPrevoteQuorum(h: Long, r: Long)(using vs: ValidatorSet): Option[BlockId] =
    quorumBlock(h, r, _.preVotes)

  /** Def. 6: unique block with a commit certificate in any round r' <= r at height h. */
  def getPrecommitQuorum(h: Long, r: Long)(using vs: ValidatorSet): Option[BlockId] =
    (0L to r).view.flatMap(rr => quorumBlock(h, rr, _.preCommits)).headOption

  /** Def. 7: threshold of validators issued nil in step s at (h, r). */
  def hasNilQuorum(h: Long, r: Long, s: Step)(using vs: ValidatorSet): Boolean =
      val select = s match
          case Prevote   => (_: RoundState).preVotes
          case Precommit => (_: RoundState).preCommits
          case Proposal  => (_: RoundState).preVotes
      votesIn(h, r, select).values.count {
        case Some(Vote(Some(_), _)) => false
        case Some(Vote(None, _))    => true
        case None                   => false
      } >= vs.quorum
}

object TendermintState:
    given Lattice[RoundState]      = Lattice.derived
    given Lattice[HeightState]     = Lattice.derived
    given Lattice[TendermintState] = Lattice.derived

    given Bottom[TendermintState] =
      Bottom.provide(TendermintState())

    /** Minimal delta containing exactly one protocol message at its coordinates
      * (Def. 1: B(t+1) = B(t) ⊔ δ(m)). The message is attributed to the sender
      * encoded in its evidence, not to the replica performing the merge.
      * TendermintState itself is the message type: sending a message means
      * producing such a delta, receiving means merging it into the lattice.
      */
    def proposal(h: Long, r: Long, p: ProposalMsg): TendermintState =
      TendermintState(Map(h -> HeightState(Map(r -> RoundState(proposals = Set(p))))))

    def prevote(h: Long, r: Long, v: Vote): TendermintState =
      TendermintState(Map(h -> HeightState(Map(r -> RoundState(preVotes = Map(v.evidence.sender -> Set(v)))))))

    def precommit(h: Long, r: Long, v: Vote): TendermintState =
      TendermintState(Map(h -> HeightState(Map(r -> RoundState(preCommits = Map(v.evidence.sender -> Set(v)))))))
