package rdts.protocols.chimeric

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.Agreement.*
import rdts.protocols.{BallotNum, Consensus, Participants, PaxosRound, Voting}

case class ChimericOpen[A](
    network: OpenNetwork,
    rounds: Map[(ConfigId, BallotNum), PaxosRound[A]] = Map.empty
):

  import ChimericOpen.given

  private def activeConfigId: ConfigId = network.currentConfigId

  def currentRound: Option[((ConfigId, BallotNum), PaxosRound[A])] =
    rounds.maxByOption(_._1)(using summon[Ordering[(ConfigId, BallotNum)]])

  def currentBallotNum: BallotNum =
    currentRound.map(_._1._2).get

  def currentLeaderElection: Option[Voting[Uid]] =
    currentRound.map(_._2.leaderElection)

  def leaderCandidate: Uid =
    currentLeaderElection.flatMap(_.votes.headOption).map(_.value).get

  def lastValueVote: Option[((ConfigId, BallotNum), PaxosRound[A])] =
    rounds
      .filter(_._2.proposals.votes.nonEmpty)
      .maxByOption(_._1)(using summon[Ordering[(ConfigId, BallotNum)]])

  def newestReceivedVal: Option[A] =
    lastValueVote.flatMap(_._2.proposals.votes.headOption).map(_.value)

  def myValue(using LocalUid): Option[A] =
    rounds
      .get((activeConfigId, BallotNum(replicaId, -1)))
      .flatMap(_.proposals.votes.headOption)
      .map(_.value)

  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter = rounds.keys.map(_._2.counter).maxOption.getOrElse(-1L)
    BallotNum(replicaId, maxCounter + 1)

  def voteLeader(leader: Uid)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      leaderElection =
        currentRound
          .map(_._2.leaderElection)
          .getOrElse(Voting[Uid]())
          .voteFor(leader)
    )

  def voteValue(value: A)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      proposals =
        currentRound
          .map(_._2.proposals)
          .getOrElse(Voting[A]())
          .voteFor(value)
    )

  def currentRoundHasCandidate: Boolean =
    currentRound.exists { case (_, PaxosRound(leaderElection, _)) =>
      leaderElection.votes.nonEmpty
    }

  def currentRoundHasProposal: Boolean =
    currentRound.exists { case (_, PaxosRound(_, proposals)) =>
      proposals.votes.nonEmpty
    }

  def isCurrentLeader(using LocalUid): Boolean =
    currentRound match
      case Some(((cfgId, _), PaxosRound(leaderElection, _))) =>
        leaderDecision(leaderElection)(using network.config(cfgId).slices) match
          case Decided(l) => l == replicaId
          case _          => false
      case None =>
        false

  def phase1a(value: A)(using LocalUid): ChimericOpen[A] =
    copy(
      rounds = rounds ++ Map(
        (activeConfigId, nextBallotNum) -> voteLeader(replicaId),
        (activeConfigId, BallotNum(replicaId, -1)) ->
          PaxosRound(
            proposals = Voting[A]().voteFor(value)
          )
      )
    )

  def phase1a(using LocalUid): ChimericOpen[A] =
    copy(
      rounds = rounds ++ Map(
        (activeConfigId, nextBallotNum) -> voteLeader(replicaId)
      )
    )

  def phase1b(using LocalUid): ChimericOpen[A] =
    if !currentRoundHasCandidate then this
    else
      lastValueVote match
        case Some((promisedBallot, acceptedVal)) =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) -> voteLeader(leaderCandidate),
              promisedBallot -> acceptedVal
            )
          )
        case None =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) -> voteLeader(leaderCandidate)
            )
          )

  def phase2a(myProposal: A)(using LocalUid, Participants): ChimericOpen[A] =
    if !isCurrentLeader then this
    else
      newestReceivedVal match
        case Some(v) =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) -> voteValue(v)
            )
          )
        case None =>
          copy(
            rounds = rounds ++ Map(
              (activeConfigId, currentBallotNum) -> voteValue(myProposal)
            )
          )

  def phase2a(using LocalUid, Participants): ChimericOpen[A] =
    myValue match
      case Some(v) => phase2a(v)
      case None    => this

  def phase2b(using LocalUid): ChimericOpen[A] =
    if !currentRoundHasProposal then this
    else
      val proposal = currentRound.get._2.proposals.votes.head.value
      copy(
        rounds = rounds ++ Map(
          (activeConfigId, currentBallotNum) -> voteValue(proposal)
        )
      )

  def proposeReconfiguration(next: NetworkConfig)(using LocalUid): ChimericOpen[A] =
    if !FBASOpen.isSafeTransition(network.currentConfig.slices, next.slices) then this
    else
      this

  def reconfigurationDecision: Option[NetworkConfig] =
    None

  def enactReconfiguration: ChimericOpen[A] =
    this

  def decision(using Participants): Agreement[A] =
    rounds.toList
      .sortBy(_._1)(using summon[Ordering[(ConfigId, BallotNum)]])
      .reverse
      .collectFirst {
        case ((cfgId, _), PaxosRound(_, proposals)) =>
          proposalDecision(proposals)(using network.config(cfgId).slices)
      }
      .getOrElse(Undecided)

  private def leaderDecision(leaderElection: Voting[Uid])(using QuorumConfig): Agreement[Uid] =
    leaderElection.votes
      .groupBy(_.value)
      .collectFirst {
        case (candidate, votes)
            if FBASOpen.isQuorumReached(summon[QuorumConfig], votes.map(_.voter).toSet) =>
          Decided(candidate)
      }
      .getOrElse(Undecided)

  private def proposalDecision(proposals: Voting[A])(using QuorumConfig): Agreement[A] =
    proposals.votes
      .groupBy(_.value)
      .collectFirst {
        case (value, votes)
            if FBASOpen.isQuorumReached(summon[QuorumConfig], votes.map(_.voter).toSet) =>
          Decided(value)
      }
      .getOrElse(Undecided)

object ChimericOpen:

  given Ordering[BallotNum] with
    override def compare(x: BallotNum, y: BallotNum): Int =
      if x.counter > y.counter then 1
      else if x.counter < y.counter then -1
      else Ordering[Uid].compare(x.uid, y.uid)

  given Ordering[(ConfigId, BallotNum)] with
    override def compare(x: (ConfigId, BallotNum), y: (ConfigId, BallotNum)): Int =
      val cfgCmp = Ordering[Long].compare(x._1, y._1)
      if cfgCmp != 0 then cfgCmp
      else summon[Ordering[BallotNum]].compare(x._2, y._2)

  given [A]: Lattice[PaxosRound[A]] = Lattice.derived
  given [A]: Lattice[ChimericOpen[A]] = Lattice.derived

  given [A]: Bottom[ChimericOpen[A]] with
    override def empty: ChimericOpen[A] =
      throw new IllegalStateException("ChimericOpen.empty requires an explicit bootstrap OpenNetwork")

  def bootstrap[A](initial: NetworkConfig): ChimericOpen[A] =
    ChimericOpen[A](
      network = OpenNetwork.bootstrap(initial),
      rounds = Map.empty[(ConfigId, BallotNum), PaxosRound[A]]
    )

  given consensus: Consensus[ChimericOpen] with
    extension [A](c: ChimericOpen[A])
      override def propose(value: A)(using LocalUid, Participants): ChimericOpen[A] =
        c.phase1a(value)

    extension [A](c: ChimericOpen[A])(using Participants)
      override def result: Option[A] =
        c.decision match
          case Invalid    => None
          case Decided(x) => Some(x)
          case Undecided  => None

    extension [A](c: ChimericOpen[A])
      override def upkeep()(using LocalUid, Participants): ChimericOpen[A] =
        c.currentRound match
          case Some(((cfgId, _), PaxosRound(leaderElection, _)))
              if c.leaderDecision(leaderElection)(using c.network.config(cfgId).slices) != Undecided =>
            c.leaderDecision(leaderElection)(using c.network.config(cfgId).slices) match
              case Decided(l) if l == replicaId => c.phase2a
              case Decided(_)                   => c.phase2b
              case _                            => c.phase1b
          case _ =>
            c.phase1b

    override def empty[A]: ChimericOpen[A] =
      summon[Bottom[ChimericOpen[A]]].empty

    override def lattice[A]: Lattice[ChimericOpen[A]] =
      summon[Lattice[ChimericOpen[A]]]