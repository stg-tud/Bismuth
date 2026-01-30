package rdts.protocols.paper

import rdts.base.Lattice.syntax
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.protocols.Participants
import rdts.protocols.paper.ClosingCons.{Done, Open}
import rdts.protocols.paper.Paxos.given
import rdts.time.Time

import scala.collection.immutable.NumericRange

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
    lazy val openRounds = log.collect { case (k, ClosingCons.Open(paxos)) => (k, paxos) }
    private lazy val maxPaxos: (round: Time, paxos: Paxos[A]) = openRounds.maxByOption(_._1).getOrElse((-1, Paxos[A]()))
    lazy val nextDecisionRound: Long                          = openRounds.minByOption(_._1).map(_._1).getOrElse(-1)
    lazy val maxRound                                         = log.keys.maxOption.getOrElse(-1L)
    def nextIdleRound(using Participants): Option[(round: Time, paxos: Paxos[A])] =
      openRounds.find((_, p) => p.phase == MultipaxosPhase.Idle)

    lazy val closedRounds: Map[Long, A] = log.collect { case (k, ClosingCons.Done(value)) => (k, value) }

    def phase(using Participants) = maxPaxos.paxos.phase

    // public API
    def leader(using Participants): Option[Uid] = maxPaxos.paxos.currentRound match
        case Some(PaxosRound(leaderElection, _)) => leaderElection.result
        case None                                => None

    def readDecisionsSince(time: Time): Iterable[A] =
      NumericRange(time, nextDecisionRound, 1L).view.flatMap(log.get).collect { case Done(value) => value }

    def startLeaderElection(using LocalUid): PipePaxos[A] =
      PipePaxos(openRounds.view.mapValues(v => ClosingCons.Open(v.phase1a)).toMap)

    def proposeIfLeader(value: A)(using LocalUid, Participants): PipePaxos[A] =
      nextIdleRound match {
        case Some(round) =>
          PipePaxos(
            Map(round.round -> Open(round.paxos.phase2a(value)))
          ) // phase 2a already checks if I am the leader
        case None =>
          val rounded = addRound()
          val proposed = rounded.maxPaxos.paxos.phase2a(value)
          rounded `merge` PipePaxos(Map(rounded.maxRound -> Open(proposed)))

      }

    def addRound()(using Participants): PipePaxos[A] = {
      val maybeshortcut = maxPaxos.paxos.newestBallotWithLeader match {
        case Some((ballotNum, PaxosRound(leaderElection, _))) =>
          Paxos(rounds =
            Map(ballotNum -> PaxosRound(
              leaderElection = leaderElection,
              proposals = Voting[A]()
            ))
          )
        case _ => Paxos[A]()
      }
      PipePaxos(Map((maxPaxos.round + 1) -> Open(maybeshortcut)))
    }

    def upkeep(using LocalUid, Participants): PipePaxos[A] =
        val res = PipePaxos {
          openRounds.map { (roundNumber, paxos) =>
            // perform upkeep in Paxos
            val deltaPaxos = paxos.upkeep()
            val newPaxos   = paxos.merge(deltaPaxos)

            newPaxos.result match
                case Some(decision) =>
                  // we are voting on proposals and there is a decision
                  Map(roundNumber -> Done(decision))
                case _ =>
                  // nothing to do, return upkeep result
                  Map(roundNumber -> Open(deltaPaxos))
          }.foldLeft(Map.empty[Long, ClosingCons[A]])(Lattice.merge[Map[Long, ClosingCons[A]]])
        }
        // keep an open round to remember leaderelection
        if res.openRounds.nonEmpty
        then res
        else
            res `merge` addRound()

object PipePaxos:
    def empty[A]: PipePaxos[A] = PipePaxos[A](Map.empty)

    given [A] => Lattice[PipePaxos[A]] = Lattice.derived
