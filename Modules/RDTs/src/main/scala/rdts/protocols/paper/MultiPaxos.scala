package rdts.protocols.paper

import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.Epoch
import Paxos.given
import rdts.base.LocalUid.replicaId
import rdts.protocols.Participants
import rdts.time.Time

import scala.collection.immutable.NumericRange

enum MultipaxosPhase:
  case LeaderElection
  case Voting
  case Idle

case class MultiPaxos[A](
    rounds: Epoch[Paxos[A]] = Epoch.empty[Paxos[A]],
    log: Map[Long, A] = Map.empty
):

  // private helper functions
  private def currentPaxos = rounds.value

  // public API
  def leader(using Participants): Option[Uid] = currentPaxos.currentRound match
    case Some(PaxosRound(leaderElection, _)) => leaderElection.result
    case None                                => None

  def phase(using Participants): MultipaxosPhase =
    currentPaxos.currentRound match
      case None                                                                 => MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, _)) if leaderElection.result.isEmpty => MultipaxosPhase.LeaderElection
      case Some(PaxosRound(leaderElection, proposals))
          if leaderElection.result.nonEmpty && proposals.votes.nonEmpty => MultipaxosPhase.Voting
      case Some(PaxosRound(leaderElection, proposals))
          if leaderElection.result.nonEmpty && proposals.votes.isEmpty => MultipaxosPhase.Idle
      case _ => throw new Error("Inconsistent Paxos State")

  def read: List[A]                               = log.toList.sortBy(_._1).map(_._2)
  def readDecisionsSince(time: Time): Iterable[A] =
    NumericRange(time, rounds.counter, 1L).view.flatMap(log.get)

  def startLeaderElection(using LocalUid, Participants): MultiPaxos[A] =
    MultiPaxos(rounds.write(currentPaxos.phase1a)) // start new Paxos round with self proposed as leader

  def proposeIfLeader(value: A)(using LocalUid, Participants): MultiPaxos[A] =
    MultiPaxos(rounds = rounds.write(currentPaxos.phase2a(value))) // phase 2a already checks if I am the leader

  def upkeep(using LocalUid, Participants): MultiPaxos[A] = {
    // perform upkeep in Paxos
    val deltaPaxos = currentPaxos.upkeep()
    val newPaxos   = currentPaxos.merge(deltaPaxos)

    (newPaxos.result, newPaxos.newestBallotWithLeader) match
      case (Some(decision), Some((ballotNum, PaxosRound(leaderElection, _)))) =>
        // we are voting on proposals and there is a decision

        val newLog = Map(rounds.counter -> decision) // append log
        // create new Paxos where leader is already elected
        val newPaxos = Paxos(rounds =
          Map(ballotNum -> PaxosRound(
            leaderElection = leaderElection,
            proposals = Voting[A]()
          ))
        )
        // return new Multipaxos with: appended log
        MultiPaxos(
          rounds = rounds.epocheWrite(newPaxos),
          log = newLog
        )
      case _ =>
        // nothing to do, return upkeep result
        MultiPaxos(rounds = rounds.write(deltaPaxos))
  }

  override def toString: String =
    lazy val s = s"MultiPaxos(epoch: $rounds, log: $read)"
    s

object MultiPaxos:
  def empty[A]: MultiPaxos[A] = MultiPaxos[A]()

  given [A]: Lattice[MultiPaxos[A]] =
    // for the log
    given Lattice[Map[Long, A]] =
      given Lattice[A] = Lattice.assertEquals
      Lattice.mapLattice
    Lattice.derived

  given [A]: Bottom[MultiPaxos[A]] = Bottom.provide(empty)
