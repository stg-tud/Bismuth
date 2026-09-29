package rdts.protocols

import rdts.base.{Bottom, Lattice, ReplicaId, Uid}
import rdts.protocols.Paxos.given
import rdts.protocols.Util.*
import rdts.protocols.Util.Agreement.*
import rdts.protocols.{Consensus, Participants}

// Paxos PRDT
type LeaderElection = Voting[Uid]
case class PaxosRound[A](
    leaderElection: LeaderElection =
      Voting(Set.empty[Vote[Uid]]),
    proposals: Voting[A] = Voting[A](Set.empty[Vote[A]])
)
case class BallotNum(uid: Uid, counter: Long)

case class Paxos[A](
    rounds: Map[BallotNum, PaxosRound[A]] =
      Map.empty[BallotNum, PaxosRound[A]]
) {

  def phase(using Participants): MultipaxosPhase = currentRound match
      case None                                                                 => MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, _)) if leaderElection.result.isEmpty => MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, proposals))
          if leaderElection.result.nonEmpty && proposals.votes.nonEmpty => MultipaxosPhase.Voting
      case Some(PaxosRound(leaderElection, proposals))
          if leaderElection.result.nonEmpty && proposals.votes.isEmpty => MultipaxosPhase.Idle
      case _ => throw new Error("Inconsistent Paxos State")

  // voting
  def voteLeader(leader: Uid)(using
      ReplicaId
  ): PaxosRound[A] =
    PaxosRound(leaderElection =
      currentRound.getOrElse(PaxosRound()).leaderElection.voteFor(leader)
    )
  def voteValue(value: A)(using
      ReplicaId
  ): PaxosRound[A] =
    PaxosRound(proposals =
      currentRound.getOrElse(PaxosRound()).proposals.voteFor(value)
    )

  // preconditions
  def roundHasCandidate(ballotNum: BallotNum, candidate: Uid): Boolean = rounds.get(ballotNum) match
      case Some(PaxosRound(leaderElection, _))
          if leaderElection.votes.exists(_.value == candidate) => true
      case _ => false
  def currentRoundHasCandidate: Boolean = currentRound match
      case Some(PaxosRound(leaderElection, _))
          if leaderElection.votes.nonEmpty => true
      case _ => false
  def isLeaderInRound(ballotNum: BallotNum)(using
      p: Participants,
      replicaId: ReplicaId
  ): Boolean = rounds.get(ballotNum) match
      case Some(PaxosRound(leaderElection, _))
          if leaderElection.decision == Decided(replicaId.uid) =>
        true
      case _ => false
  def isCurrentLeader(using
      participants: Participants,
      replicaId: ReplicaId
  ): Boolean = currentRound match
      case Some(PaxosRound(leaderElection, _))
          if leaderElection.decision == Decided(replicaId.uid) =>
        true
      case _ => false
  def roundHasProposal(ballotNum: BallotNum, proposal: A): Boolean =
    rounds.get(ballotNum).map(_.proposals.votes.exists(_.value == proposal)).getOrElse(false)
  def currentRoundHasProposal: Boolean = currentRound match
      case Some(PaxosRound(_, proposals))
          if proposals.votes.nonEmpty => true
      case _ => false

  // protocol actions:
  // actual protocol action:
  def phase1a(b: BallotNum)(using replicaId: ReplicaId): Paxos[A] =
    // try to become leader
    Paxos(Map(b -> PaxosRound(leaderElection = Voting().voteFor(replicaId.uid))))
  def phase1a(b: BallotNum, value: A)(using replicaId: ReplicaId): Paxos[A] =
    Paxos(Map(
      b                            -> PaxosRound(leaderElection = Voting().voteFor(replicaId.uid)),
      BallotNum(replicaId.uid, -1) -> PaxosRound(proposals = Voting().voteFor(value))
    ))
  // api:
  def phase1a(using ReplicaId): Paxos[A] =
    phase1a(nextBallotNum)
  def phase1a(value: A)(using ReplicaId): Paxos[A] =
    phase1a(nextBallotNum, value)

  // actual protocol action:
  def phase1b(
      currentBallotNum: BallotNum,
      currentLeaderelection: LeaderElection,
      currentCandidate: Uid,
      lastPromise: Option[(BallotNum, PaxosRound[A])]
  )(using
      ReplicaId
  ): Paxos[A] =
    precondition(
      roundHasCandidate(currentBallotNum, currentCandidate) &&
      rounds(currentBallotNum).leaderElection.subsumes(currentLeaderelection)
    )(
      // vote in the current leader election
      lastPromise match
          case Some(promisedBallot, acceptedVal) =>
            // vote for candidate and include value most recently voted for
            Paxos(Map(
              currentBallotNum -> PaxosRound(leaderElection = currentLeaderelection.voteFor(currentCandidate)),
              promisedBallot   -> acceptedVal // previously accepted value
            ))
          case None =>
            // no value voted for, just vote for candidate
            Paxos(Map(
              currentBallotNum -> PaxosRound(leaderElection = currentLeaderelection.voteFor(currentCandidate))
            ))
    )
  // api:
  def phase1b(using replicaId: ReplicaId): Paxos[A] =
    phase1b(
      currentBallotNum.getOrElse(BallotNum(replicaId.uid, -1)),
      currentLeaderElection.getOrElse(Voting()),
      lastPromise = lastValueVote,
      currentCandidate = leaderCandidate.getOrElse(replicaId.uid)
    )

  // actual protocol action:
  def phase2a(myValue: A, currentBallotNum: BallotNum, currentProposals: Voting[A], newestReceivedVal: Option[A])(using
      ReplicaId,
      Participants
  ): Paxos[A] =
    // propose a value if I am the leader
    precondition(
      isLeaderInRound(currentBallotNum) &&
      rounds(currentBallotNum).proposals.subsumes(currentProposals)
    ) {
      newestReceivedVal match
          case Some(value) =>
            // propose most recent received value
            Paxos(Map(currentBallotNum -> PaxosRound(proposals = currentProposals.voteFor(value))))
          case None =>
            Paxos(Map(currentBallotNum -> PaxosRound(proposals = currentProposals.voteFor(myValue))))
    }
  // api:
  def phase2a(myValue: A)(using ReplicaId, Participants): Paxos[A] =
    (currentBallotNum, currentProposals) match
        case (Some(b), Some(ps)) =>
          phase2a(
            myValue = myValue,
            currentBallotNum = b,
            currentProposals = ps,
            newestReceivedVal = newestReceivedVal
          )
        case _ => Paxos() // don't do anything
  // This is a helper function that allows calling phase2a without a parameter.
  // In this case myValue has to be known from context, otherwise this does nothing.
  def phase2a(using ReplicaId, Participants): Paxos[A] =
    myValue match
        case Some(m) =>
          phase2a(m)
        case None => Paxos() // don't do anything

  // actual protocol action:
  def phase2b(currentBallotNum: BallotNum, proposal: A, currentProposals: Voting[A])(using ReplicaId): Paxos[A] =
    precondition(
      roundHasProposal(currentBallotNum, proposal) &&
      rounds(currentBallotNum).proposals.subsumes(currentProposals)
    ) {
      Paxos(Map(currentBallotNum -> PaxosRound(
        proposals = currentProposals.voteFor(proposal)
      )))
    }
  // api:
  def phase2b(using ReplicaId): Paxos[A] =
    (currentBallotNum, currentProposal, currentProposals) match
        case (Some(b), Some(p), Some(ps)) =>
          phase2b(
            currentBallotNum = b,
            proposal = p,
            currentProposals = ps
          )
        case _ => Paxos()

  // decision function
  def decision(using Participants): Agreement[A] =
    rounds.collectFirst {
      case (b, PaxosRound(_, proposals))
          if proposals.decision != Undecided =>
        proposals.decision
    }.getOrElse(Undecided)

  // helper functions
  def nextBallotNum(using replicaId: ReplicaId): BallotNum =
      val maxCounter: Long = rounds
        .map((b, _) => b.counter)
        .maxOption
        .getOrElse(-1)
      BallotNum(replicaId.uid, maxCounter + 1)
  def currentRound: Option[PaxosRound[A]] =
    rounds.maxOption.map(_._2)
  def currentBallotNum: Option[BallotNum] =
    rounds.maxOption.map(_._1)
  def currentProposals: Option[Voting[A]] =
    currentRound.map(_.proposals)
  def currentProposal: Option[A] =
    currentProposals.flatMap(_.votes.headOption).map(_.value)
  def leaderCandidate: Option[Uid] =
    currentLeaderElection.flatMap(_.votes.headOption).map(_.value)
  def currentLeaderElection: Option[LeaderElection] =
    currentRound match
        case Some(PaxosRound(leaderElection, _)) =>
          Some(leaderElection)
        case None => None
  def lastValueVote: Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.proposals.votes.nonEmpty).maxOption
  def newestReceivedVal: Option[A] =
    lastValueVote.flatMap(_._2.proposals.votes.headOption).map(_.value)
  def myValue(using replicaId: ReplicaId): Option[A] = rounds.get(BallotNum(
    replicaId.uid,
    -1
  )).flatMap(_.proposals.votes.headOption).map(_.value)
  def newestBallotWithLeader(using Participants): Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.leaderElection.result.nonEmpty).maxOption
}

object Paxos {
  given [A]: Lattice[PaxosRound[A]]        = Lattice.derived
  given paxosLattice[A]: Lattice[Paxos[A]] = Lattice.derived
  given paxosBottom[A]: Bottom[Paxos[A]]   = Bottom.provide(Paxos())

  given Ordering[BallotNum] with
      override def compare(x: BallotNum, y: BallotNum): Int =
        if x.counter > y.counter then 1
        else if x.counter < y.counter then -1
        else Ordering[Uid].compare(x.uid, y.uid)
  given [A]: Ordering[(BallotNum, PaxosRound[A])] with
      override def compare(
          x: (BallotNum, PaxosRound[A]),
          y: (BallotNum, PaxosRound[A])
      ): Int = (x, y) match
          case ((x, _), (y, _)) =>
            Ordering[BallotNum].compare(x, y)

  // implementation of consensus typeclass for the testing framework
  given consensus: Consensus[Paxos] with
      extension [A](c: Paxos[A])
          override def propose(value: A)(using ReplicaId, Participants): Paxos[A] =
              // check if I can propose a value
              val afterProposal = c.phase2a
              if Lattice.subsumption(afterProposal, c) then
                  // proposing did not work, try to become leader
                  c.phase1a(value)
              else
                  afterProposal
      extension [A](c: Paxos[A])(using Participants)
          override def result: Option[A] = c.decision match {
            case Invalid                       => None
            case Util.Agreement.Decided(value) => Some(value)
            case Util.Agreement.Undecided      => None
          }
      extension [A](c: Paxos[A])
          // upkeep can be used to perform the next protocol step automatically
          override def upkeep()(using replicaId: ReplicaId, participants: Participants): Paxos[A] =
            // check which phase we are in
            c.currentRound match
                case Some(PaxosRound(leaderElection, _)) if leaderElection.result.nonEmpty =>
                  // we have a leader -> phase 2
                  if leaderElection.result.get == replicaId.uid then
                      c.phase2a
                  else
                      c.phase2b
                // we are in the process of electing a new leader
                case _ =>
                  c.phase1b

      override def empty[A]: Paxos[A] = paxosBottom.empty

      override def lattice[A]: Lattice[Paxos[A]] = paxosLattice

}
