package rdts.protocols.chimeric

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Util.*
import rdts.protocols.Util.Agreement.*
import rdts.protocols.{Consensus, Participants}
import rdts.protocols.{BallotNum, PaxosRound, Voting, MultipaxosPhase}
import rdts.protocols.Paxos.given

case class Chimeric[A](
    rounds: Map[BallotNum, PaxosRound[A]] =
      Map.empty[BallotNum, PaxosRound[A]]
):

  def phase(using QuorumConfig): MultipaxosPhase =
    currentRound match
      case None => MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, _)) if leaderDecision(leaderElection) == Undecided =>
        MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, proposals))
          if leaderDecision(leaderElection) != Undecided && proposals.votes.nonEmpty =>
        MultipaxosPhase.Voting
      case Some(PaxosRound(leaderElection, proposals))
          if leaderDecision(leaderElection) != Undecided && proposals.votes.isEmpty =>
        MultipaxosPhase.Idle
      case _ =>
        throw new Error("Inconsistent Chimeric State")

  def voteLeader(leader: Uid)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      leaderElection =
        currentRound.getOrElse(PaxosRound()).leaderElection.voteFor(leader)
    )

  def voteValue(value: A)(using LocalUid): PaxosRound[A] =
    PaxosRound(
      proposals =
        currentRound.getOrElse(PaxosRound()).proposals.voteFor(value)
    )

  def currentRoundHasCandidate: Boolean =
    currentRound match
      case Some(PaxosRound(leaderElection, _)) if leaderElection.votes.nonEmpty => true
      case _ => false

  def isCurrentLeader(using LocalUid, QuorumConfig): Boolean =
    currentRound match
      case Some(PaxosRound(leaderElection, _)) =>
        leaderDecision(leaderElection) match
          case Decided(leader) => leader == replicaId
          case _               => false
      case None => false

  def currentRoundHasProposal: Boolean =
    currentRound match
      case Some(PaxosRound(_, proposals)) if proposals.votes.nonEmpty => true
      case _ => false

  def phase1a(using LocalUid)(value: A): Chimeric[A] =
    Chimeric(
      Map(
        nextBallotNum -> voteLeader(replicaId),
        BallotNum(replicaId, -1) -> voteValue(value)
      )
    )

  def phase1a(using LocalUid): Chimeric[A] =
    Chimeric(Map(nextBallotNum -> voteLeader(replicaId)))

  def phase1b(using LocalUid): Chimeric[A] =
    precondition(currentRoundHasCandidate)(
      lastValueVote match
        case Some((promisedBallot, acceptedVal)) =>
          Chimeric(
            Map(
              currentBallotNum -> voteLeader(leaderCandidate),
              promisedBallot   -> acceptedVal
            )
          )
        case None =>
          Chimeric(
            Map(currentBallotNum -> voteLeader(leaderCandidate))
          )
    )

  def phase2a(myVal: A)(using LocalUid, QuorumConfig): Chimeric[A] =
    precondition(isCurrentLeader)(
      if newestReceivedVal.nonEmpty then
        Chimeric(Map(currentBallotNum -> voteValue(newestReceivedVal.get)))
      else
        Chimeric(Map(currentBallotNum -> voteValue(myVal)))
    )

  def phase2a(using LocalUid, QuorumConfig): Chimeric[A] =
    precondition(myValue.nonEmpty) {
      phase2a(myValue.get)
    }

  def phase2b(using LocalUid): Chimeric[A] =
    precondition(currentRoundHasProposal) {
      val proposal = currentRound.get.proposals.votes.head.value
      Chimeric(Map(currentBallotNum -> voteValue(proposal)))
    }

  def decision(using QuorumConfig): Agreement[A] =
    rounds.toList
      .sortBy(_._1)
      .reverse
      .collectFirst {
        case (_, PaxosRound(_, proposals))
            if proposalDecision(proposals) != Agreement.Undecided =>
          proposalDecision(proposals)
      }
      .getOrElse(Agreement.Undecided)

  private def leaderDecision(leaderElection: Voting[Uid])(using QuorumConfig): Agreement[Uid] =
    leaderElection.votes
      .groupBy(_.value)
      .collectFirst {
        case (candidate, votes)
            if FBAS.isQuorumReached(summon[QuorumConfig], votes.map(_.voter).toSet) =>
          Agreement.Decided(candidate)
      }
      .getOrElse(Agreement.Undecided)

  private def proposalDecision(proposals: Voting[A])(using QuorumConfig): Agreement[A] =
    proposals.votes
      .groupBy(_.value)
      .collectFirst {
        case (value, votes)
            if FBAS.isQuorumReached(summon[QuorumConfig], votes.map(_.voter).toSet) =>
          Agreement.Decided(value)
      }
      .getOrElse(Agreement.Undecided)

  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter: Long =
      rounds.map((b, _) => b.counter).maxOption.getOrElse(-1)
    BallotNum(replicaId, maxCounter + 1)

  def currentRound: Option[PaxosRound[A]] =
    rounds.maxOption.map(_._2)

  def currentBallotNum: BallotNum =
    rounds.maxOption.map(_._1).get

  def leaderCandidate: Uid =
    currentLeaderElection.map(_.votes.head.value).get

  def currentLeaderElection: Option[Voting[Uid]] =
    currentRound match
      case Some(PaxosRound(leaderElection, _)) => Some(leaderElection)
      case None                                => None

  def lastValueVote: Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.proposals.votes.nonEmpty).maxOption

  def newestReceivedVal: Option[A] =
    lastValueVote.flatMap(_._2.proposals.votes.headOption).map(_.value)

  def myValue(using LocalUid): Option[A] =
    rounds
      .get(BallotNum(replicaId, -1))
      .flatMap(_.proposals.votes.headOption)
      .map(_.value)

  def newestBallotWithLeader(using QuorumConfig): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter { case (_, PaxosRound(leaderElection, _)) =>
      leaderDecision(leaderElection) != Undecided
    }.maxOption

object Chimeric:

  given [A]: Lattice[PaxosRound[A]] = Lattice.derived
  given chimericLattice[A]: Lattice[Chimeric[A]] = Lattice.derived
  given chimericBottom[A]: Bottom[Chimeric[A]] = Bottom.provide(Chimeric())

  given Ordering[BallotNum] with
    override def compare(x: BallotNum, y: BallotNum): Int =
      if x.counter > y.counter then 1
      else if x.counter < y.counter then -1
      else Ordering[Uid].compare(x.uid, y.uid)

  given [A]: Ordering[(BallotNum, PaxosRound[A])] with
    override def compare(
        x: (BallotNum, PaxosRound[A]),
        y: (BallotNum, PaxosRound[A])
    ): Int =
      Ordering[BallotNum].compare(x._1, y._1)

  given consensus(using QuorumConfig): Consensus[Chimeric] with
    extension [A](c: Chimeric[A])
      override def propose(value: A)(using LocalUid, Participants): Chimeric[A] =
        val afterProposal = c.phase2a
        if Lattice.subsumption(afterProposal, c) then
          c.phase1a(value)
        else
          afterProposal

    extension [A](c: Chimeric[A])(using Participants)
      override def result: Option[A] =
        c.decision(using  summon[QuorumConfig]) match
          case Agreement.Invalid   => None
          case Agreement.Decided(v) => Some(v)
          case Agreement.Undecided => None

    extension [A](c: Chimeric[A])
      override def upkeep()(using LocalUid, Participants): Chimeric[A] =
        c.currentRound match
          case Some(PaxosRound(leaderElection, _))
              if c.leaderDecision(leaderElection) != Undecided =>
            c.leaderDecision(leaderElection) match
              case Decided(leader) if leader == replicaId =>
                c.phase2a
              case Decided(_) =>
                c.phase2b
              case _ =>
                c.phase1b
          case _ =>
            c.phase1b

    override def empty[A]: Chimeric[A] =
      chimericBottom.empty

    override def lattice[A]: Lattice[Chimeric[A]] =
      chimericLattice