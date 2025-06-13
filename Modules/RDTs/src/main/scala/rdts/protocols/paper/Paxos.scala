package rdts.protocols.paper

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.{Consensus, Participants}
import rdts.protocols.paper.Paxos.given
import rdts.protocols.paper.Util.*
import rdts.protocols.paper.Util.Agreement.*

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
  // voting
  def voteLeader(leader: Uid)(using
      LocalUid,
      Participants
  ): PaxosRound[A] =
    PaxosRound(leaderElection =
      currentRound.getOrElse(PaxosRound()).leaderElection.voteFor(leader)
    )
  def voteValue(value: A)(using
      LocalUid,
      Participants
  ): PaxosRound[A] =
    PaxosRound(proposals =
      currentRound.getOrElse(PaxosRound()).proposals.voteFor(value)
    )

  // boolean threshold queries
  def currentRoundHasCandidate: Boolean = currentRound match
    case Some(PaxosRound(leaderElection, _))
        if leaderElection.votes.nonEmpty => true
    case _ => false
  def isCurrentLeader(using
      Participants,
      LocalUid
  ): Boolean = currentRound match
    case Some(PaxosRound(leaderElection, _))
        if leaderElection.decision == Decided(replicaId) =>
      true
    case _ => false
  def currentRoundHasProposal: Boolean = currentRound match
    case Some(PaxosRound(_, proposals))
        if proposals.votes.nonEmpty => true
    case _ => false

  // protocol actions:
  def phase1a(using LocalUid, Participants)(value: A): Paxos[A] =
    // try to become leader and remember a value for later
    Paxos(Map(nextBallotNum -> voteLeader(replicaId), BallotNum(replicaId, -1) -> voteValue(value)))
  def phase1a(using LocalUid, Participants): Paxos[A] =
    // try to become leader
    Paxos(Map(nextBallotNum -> voteLeader(replicaId)))

  def phase1b(using LocalUid, Participants): Paxos[A] =
    updateIf(currentRoundHasCandidate)(
      // vote in the current leader election
      lastValueVote match
        case Some(promisedBallot, acceptedVal) =>
          // vote for candidate and include value most recently voted for
          Paxos(Map(
            currentBallotNum -> voteLeader(leaderCandidate),
            promisedBallot   -> acceptedVal
          )) // previously accepted value
        case None =>
          // no value voted for, just vote for candidate
          Paxos(Map(
            currentBallotNum -> voteLeader(leaderCandidate)
          ))
    )

  def phase2a(using LocalUid, Participants): Paxos[A] =
    // propose a value if I am the leader
    updateIf(isCurrentLeader)(
      if newestReceivedVal.nonEmpty then
        // propose most recent received value
        Paxos(Map(currentBallotNum -> voteValue(
          newestReceivedVal.get
        )))
      else
        // no values received during leader election, propose my value
        Paxos(Map(currentBallotNum -> voteValue(myValue)))
    )
  def phase2a(myValue: A)(using LocalUid, Participants): Paxos[A] =
    // propose a value if I am the leader
    updateIf(isCurrentLeader)(
      if newestReceivedVal.nonEmpty then
        // propose most recent received value
        Paxos(Map(currentBallotNum -> voteValue(
          newestReceivedVal.get
        )))
      else
        // no values received during leader election, propose my value
        Paxos(Map(currentBallotNum -> voteValue(myValue)))
    )

  def phase2b(using LocalUid, Participants): Paxos[A] =
    // accept proposed value
    updateIf(currentRoundHasProposal) {
      val proposal =
        currentRound.get.proposals.votes.head.value
      Paxos(Map(currentBallotNum -> voteValue(proposal)))
    }

  // decision function
  def decision(using Participants): Agreement[A] =
    rounds.collectFirst {
      case (b, PaxosRound(_, proposals))
          if proposals.decision != Undecided =>
        proposals.decision
    }.getOrElse(Undecided)

  // helper functions
  def nextBallotNum(using LocalUid): BallotNum =
    val maxCounter: Long = rounds
      .map((b, _) => b.counter)
      .maxOption
      .getOrElse(-1)
    BallotNum(replicaId, maxCounter + 1)
  def currentRound: Option[PaxosRound[A]] =
    rounds.maxOption.map(_._2)
  def currentBallotNum: BallotNum =
    rounds.maxOption.map(_._1).get
  def leaderCandidate: Uid =
    currentLeaderElection.map(_.votes.head.value).get
  def currentLeaderElection: Option[LeaderElection] =
    currentRound match
      case Some(PaxosRound(leaderElection, _)) =>
        Some(leaderElection)
      case None => None
  def lastValueVote: Option[(BallotNum, PaxosRound[A])] =
    rounds.filter(_._2.proposals.votes.nonEmpty).maxOption
  def newestReceivedVal(using LocalUid) =
    lastValueVote.map(_._2.proposals.votes.head.value)
  def myValue(using LocalUid): A = rounds(BallotNum(
    replicaId,
    -1
  )).proposals.votes.head.value
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
      override def propose(value: A)(using LocalUid, Participants): Paxos[A] =
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
      override def upkeep()(using LocalUid, Participants): Paxos[A] =
        // check which phase we are in
        c.currentRound match
          case Some(PaxosRound(leaderElection, _)) if leaderElection.result.nonEmpty =>
            // we have a leader -> phase 2
            if leaderElection.result.get == replicaId then
              c.phase2a
            else
              c.phase2b
          // we are in the process of electing a new leader
          case _ =>
            c.phase1b

    override def empty[A]: Paxos[A] = paxosBottom.empty

    override def lattice[A]: Lattice[Paxos[A]] = paxosLattice

}
