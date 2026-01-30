package rdts.protocols.paper

import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Participants
import rdts.protocols.paper.ClosingCons.{Done, Open}
import rdts.protocols.paper.Paxos.given
import rdts.time.Time

enum ClosingCons[A]:
    case Open(paxos: Paxos[A])
    case Done(value: A)
object ClosingCons:
    given [A] => Lattice[ClosingCons[A]] =
        given Lattice[Open[A]] = Lattice.derived
        given Lattice[Done[A]] = Lattice.assertEquals
        Lattice.sumLattice

case class PipePaxos[A](
    log: Map[Long, ClosingCons[A]]
):

    // private helper functions
    private lazy val openRounds = log.collect { case (k, ClosingCons.Open(paxos)) => (k, paxos) }
    private lazy val maxPaxos: (round: Time, paxos: Paxos[A]) = openRounds.maxBy(_._1)

    // public API
    def leader(using Participants): Option[Uid] = maxPaxos.paxos.currentRound match
        case Some(PaxosRound(leaderElection, _)) => leaderElection.result
        case None                                => None

    def phase(using Participants): MultipaxosPhase = MultipaxosPhase.Idle
//      openRounds.currentRound match
//          case None                                                                 => MultipaxosPhase.LeaderElection
//          case Some(PaxosRound(leaderElection, _)) if leaderElection.result.isEmpty => MultipaxosPhase.LeaderElection
//          case Some(PaxosRound(leaderElection, proposals))
//              if leaderElection.result.nonEmpty && proposals.votes.nonEmpty => MultipaxosPhase.Voting
//          case Some(PaxosRound(leaderElection, proposals))
//              if leaderElection.result.nonEmpty && proposals.votes.isEmpty => MultipaxosPhase.Idle
//          case _ => throw new Error("Inconsistent Paxos State")

    def readDecisionsSince(time: Time): Iterable[A] =
      log.view.filter(_._1 >= time).collect { case (_, ClosingCons.Done(value)) => value }

    def startLeaderElection(using LocalUid): PipePaxos[A] =
      PipePaxos(openRounds.view.mapValues(v => ClosingCons.Open(v.phase1a)).toMap)

    def proposeIfLeader(value: A)(using LocalUid, Participants): PipePaxos[A] =
      PipePaxos(
        Map(maxPaxos.round -> Open(maxPaxos.paxos.phase2a(value)))
      ) // phase 2a already checks if I am the leader

    def upkeep(using LocalUid, Participants): PipePaxos[A] = PipePaxos {
      openRounds.map { (roundNumber, paxos) =>
        // perform upkeep in Paxos
        val deltaPaxos = paxos.upkeep()
        val newPaxos   = paxos.merge(deltaPaxos)

        (newPaxos.result, newPaxos.newestBallotWithLeader) match
            case (Some(decision), Some((ballotNum, PaxosRound(leaderElection, _)))) =>
              // we are voting on proposals and there is a decision

              val newDecision = Map(roundNumber -> Done(decision))
              if roundNumber == maxPaxos.round then
                  // create new Paxos where leader is already elected
                  val newPaxos = Paxos(rounds =
                    Map(ballotNum -> PaxosRound(
                      leaderElection = leaderElection,
                      proposals = Voting[A]()
                    ))
                  )
                  newDecision.updated(roundNumber + 1, Open(newPaxos))
              else newDecision
            case _ =>
              // nothing to do, return upkeep result
              Map(roundNumber -> Open(deltaPaxos))
      }.foldLeft(Map.empty[Long, ClosingCons[A]])(Lattice.merge[Map[Long, ClosingCons[A]]])
    }

object PipePaxos:
    def empty[A]: PipePaxos[A] = PipePaxos[A](Map.empty)

    given [A]: Bottom[PipePaxos[A]] = Bottom.provide(empty)
