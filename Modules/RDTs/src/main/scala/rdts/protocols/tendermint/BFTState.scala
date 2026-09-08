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

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.base.LocalUid.replicaId
import rdts.protocols.tendermint.BFTState.given
import rdts.protocols.tendermint.Step.*

/** Execution phase within a round. */
enum Step:
    case Proposal, Prevote, Precommit

/** Trust configuration governing adversarial capabilities.
  *
  *   - Classical: digital signatures, equivocation possible, quorum 2f + 1 (n >= 3f + 1)
  *   - Tee:       hardware-enforced monotonic counters, no equivocation, quorum f + 1
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
  * In the Tendermint setting this is a plain (mock) signature; in the TenderTee
  * variant it structurally incorporates a hardware-enforced monotonic counter
  * (Def. 3). No real cryptographic verification is performed — see
  * [[MockSignature]].
  */
sealed trait Evidence:
    def counter: Option[Long] = None
case class Signed(signature: MockSignature) extends Evidence
case class TeeSigned(ctr: Long, signature: MockSignature) extends Evidence:
    override def counter: Option[Long] = Some(ctr)

/** Validator set together with the trust model determining quorum thresholds. */
case class ValidatorSet(members: Set[Uid], model: TrustModel):
    def f: Int = model match
        case TrustModel.Classical => (members.size - 1) / 3 // n >= 3f + 1
        case TrustModel.Tee       => (members.size - 1) / 2 // n >= 2f + 1
    def quorum: Int = model match
        case TrustModel.Classical => 2 * f + 1
        case TrustModel.Tee       => f + 1

/** A vote (pre-vote or pre-commit) for a block or nil, bound to evidence. */
case class Vote[E <: Evidence](block: Option[BlockId], evidence: E)

/** A proposal carrying the validRound of the proposer's lock. */
case class ProposalMsg[E <: Evidence](block: BlockId, validRound: Long, evidence: E)

/** Messages accepted by the CRDT layer. Invalid messages (e.g. proposals from
  * non-designated leaders) are filtered prior to insertion (Sec. 4).
  */
enum InMsg[E <: Evidence]:
    case Proposal(h: Long, r: Long, proposal: ProposalMsg[E])
    case Prevote(h: Long, r: Long, vote: Vote[E])
    case Precommit(h: Long, r: Long, vote: Vote[E])

    def height: Long = this match
        case InMsg.Proposal(h, _, _)  => h
        case InMsg.Prevote(h, _, _)   => h
        case InMsg.Precommit(h, _, _) => h

    def round: Long = this match
        case InMsg.Proposal(_, r, _)  => r
        case InMsg.Prevote(_, r, _)   => r
        case InMsg.Precommit(_, r, _) => r

/** CRDT replicated state for a single round (Sec. 4). */
case class RoundState[E <: Evidence](
    proposals: Set[ProposalMsg[E]] = Set.empty[ProposalMsg[E]],
    preVotes: Map[Uid, Set[Vote[E]]] = Map.empty[Uid, Set[Vote[E]]],
    preCommits: Map[Uid, Set[Vote[E]]] = Map.empty[Uid, Set[Vote[E]]]
)

case class HeightState[E <: Evidence](rounds: Map[Long, RoundState[E]] = Map.empty[Long, RoundState[E]])

/** Hierarchical join-semilattice over (height, round, step, validator) coordinates
  * (Lemma 1). Merge is key-wise map merge with set union on overlap, hence
  * associative, commutative and idempotent: replicas that incorporate the same
  * set of deltas converge (Lemma 2), independent of delivery order and duplication.
  */
case class BlockchainState[E <: Evidence](heights: Map[Long, HeightState[E]] = Map.empty[Long, HeightState[E]]) {

    /** Minimal delta containing exactly m at its coordinates (Def. 1: B(t+1) = B(t) ⊔ δ(m)). */
    def delta(m: InMsg[E])(using LocalUid): BlockchainState[E] =
        val round = m match
            case InMsg.Proposal(_, _, p)  => RoundState(proposals = Set(p))
            case InMsg.Prevote(_, _, v)   => RoundState(preVotes = Map(replicaId -> Set(v)))
            case InMsg.Precommit(_, _, v) => RoundState(preCommits = Map(replicaId -> Set(v)))
        BlockchainState(Map(m.height -> HeightState(Map(m.round -> round))))

    def merge(m: InMsg[E])(using LocalUid): BlockchainState[E] =
        Lattice.merge(this, delta(m))

    // -- Deterministic state projections (Sec. 5) ----------------------------

    def roundState(h: Long, r: Long): RoundState[E] =
        heights.get(h).flatMap(_.rounds.get(r)).getOrElse(RoundState[E]())

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
                    .map(e => (e, ev(e)))
                    .collect { case (e, t: TeeSigned) => (e, t.counter) }
                    .minByOption(_._2)
                    .map(_._1)

    /** TenderTee per-(validator, step) contiguous-prefix admission rule (Def. 3):
      * a message is admitted only if it extends the next expected counter.
      */
    def canAdmit(existing: Set[Vote[E]], vote: Vote[E])(using vs: ValidatorSet): Boolean =
        (vote.evidence, vs.model) match
            case (TeeSigned(counter, _), TrustModel.Tee) =>
                val counters = existing.collect { case Vote(_, TeeSigned(c, _)) => c }
                counters.forall(_ < counter) && counter == counters.maxOption.getOrElse(-1L) + 1
            case (Signed(_), TrustModel.Classical) => true
            case _                                 => false

    // -- Deterministic protocol queries (Sec. 6) ------------------------------

    /** Def. 4: block proposed by the designated leader for (h, r).
      * Proposals are not keyed by validator, so π is applied over the whole
      * proposal slot: a singleton yields the proposal, any conflict yields ⊥
      * (Classical) or the minimal-counter entry (Tee).
      */
    def getProposal(h: Long, r: Long)(using vs: ValidatorSet): Option[ProposalMsg[E]] =
        project(roundState(h, r).proposals)(_.evidence)

    private def votesIn(
        h: Long,
        r: Long,
        select: RoundState[E] => Map[Uid, Set[Vote[E]]],
    )(using vs: ValidatorSet): Map[Uid, Option[Vote[E]]] =
        select(roundState(h, r)).view.mapValues(entries => project(entries)(_.evidence)).toMap

    private def quorumBlock(
        h: Long,
        r: Long,
        select: RoundState[E] => Map[Uid, Set[Vote[E]]],
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
            case Prevote   => (_: RoundState[E]).preVotes
            case Precommit => (_: RoundState[E]).preCommits
            case Proposal  => (_: RoundState[E]).preVotes
        votesIn(h, r, select).values.count {
            case Some(Vote(Some(_), _)) => false
            case Some(Vote(None, _))    => true
            case None                   => false
        } >= vs.quorum
}

object BFTState:
    given [E <: Evidence]: Lattice[RoundState[E]]      = Lattice.derived
    given [E <: Evidence]: Lattice[HeightState[E]]     = Lattice.derived
    given [E <: Evidence]: Lattice[BlockchainState[E]] = Lattice.derived

    given [E <: Evidence]: Bottom[BlockchainState[E]] =
        Bottom.provide(BlockchainState[E]())

    given [E <: Evidence]: Ordering[InMsg[E]] with
        override def compare(x: InMsg[E], y: InMsg[E]): Int =
            x.height compare y.height match
                case 0 => x.round compare y.round
                case c => c
