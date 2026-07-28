package rdts.protocols.spanner

import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Paxos.given
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import rdts.protocols.MultipaxosPhase

import scala.collection.immutable.NumericRange
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.precondition

case class ParallelMultiPaxos[A](
    log: Map[Long, Paxos[A]] = Map.empty[Long, Paxos[A]],
    commitIndex: Long = -1
):

    // private helper functions
    private def currentPaxos: Option[Paxos[A]] = log.get(commitIndex + 1)

    // public API
    def leader(using Participants): Option[Uid] = currentPaxos.flatMap(_.currentLeaderElection) match
        case Some(leaderElection) => leaderElection.result
        case None                 => None

    // TODO: not sure if we should expose this...
    def phase(using Participants): MultipaxosPhase =
      currentPaxos match
          case Some(paxos) => paxos.currentRound match
                case Some(PaxosRound(leaderElection, _)) if leaderElection.result.isEmpty =>
                  MultipaxosPhase.LeaderElection
                case Some(PaxosRound(leaderElection, proposals))
                    if leaderElection.result.nonEmpty && proposals.votes.nonEmpty => MultipaxosPhase.Voting
                case Some(PaxosRound(leaderElection, proposals))
                    if leaderElection.result.nonEmpty && proposals.votes.isEmpty => MultipaxosPhase.Idle
                case _ => throw new Error("Inconsistent Paxos State")
          case None if commitIndex == -1 =>
            MultipaxosPhase.LeaderElection // first round, no previous decision, need to elect leader
          case None => MultipaxosPhase.Idle // round not yet initialized but previous round was successful

    def read(using Participants): List[A] =
      // return values in log order but only if all previous rounds are decided
      readDecisionsSince(0)

    def readDecisionsSince(time: Long)(using Participants): List[A] =
      NumericRange(time, log.size.toLong, 1L).view.flatMap(log.get)
        // .filter(_.result.isDefined)
        .takeWhile(_.result.isDefined) // return log until first undecided round
        .map(_.result.get)
        .toList

    def startLeaderElection(index: Long)(using LocalUid): ParallelMultiPaxos[A] =
      precondition(index == 0L || log.contains(index - 1)) {
        val currentPaxos = log.getOrElse(index, Paxos[A]())
        ParallelMultiPaxos(
          Map(index -> currentPaxos.phase1a)
        ) // start new Paxos round with self proposed as leader
      }

    def startLeaderElection(using LocalUid): ParallelMultiPaxos[A] =
      startLeaderElection(commitIndex + 1)

    def proposeIfLeader(index: Long, value: A)(using LocalUid, Participants): ParallelMultiPaxos[A] =
      precondition(index == 0L || log.contains(index - 1)) {
        def openNextSlot = {
          // opens a new slot for the next log entry, either by reusing the old ballot or starting a new one
          log.get(index - 1).flatMap(_.newestBallotWithLeader) match
              case Some((ballotNum, PaxosRound(leaderElection, _))) =>
                // reuse the old ballot, but empty proposals
                Paxos(rounds =
                  Map(ballotNum -> PaxosRound(
                    leaderElection = leaderElection,
                    proposals = Voting[A]()
                  ))
                )
              case None => Paxos[A]()
        }
        val paxos =
          log.getOrElse(index, openNextSlot)

        val paxosVote = paxos.phase2a(value)

        if paxosVote != Paxos() then
            ParallelMultiPaxos(
              Map(index -> paxos.merge(paxosVote)) // phase 2a already checks if I am the leader
            )
        else ParallelMultiPaxos()
      }

    def proposeIfLeader(value: A)(using LocalUid, Participants): ParallelMultiPaxos[A] =
      proposeIfLeader(commitIndex + 1, value)

    def upkeep(using LocalUid, Participants): ParallelMultiPaxos[A] = {
      // perform upkeep in open rounds
      val open        = NumericRange(commitIndex + 1, log.size.toLong, 1L).view.map(index => (index, log(index)))
      val paxosDeltas = open.map {
        case (index, paxos) => (index, paxos.upkeep())
      }.toMap
      val newLog = log.merge(paxosDeltas)

      // move commit index
      val committed = NumericRange(commitIndex + 1, log.size.toLong, 1L).view.flatMap(newLog.get)
        .takeWhile(_.result.isDefined) // return log until first undecided round

      ParallelMultiPaxos(
        log = paxosDeltas,
        commitIndex = commitIndex + committed.size.toLong
      )
    }

    def decision(using Participants): Agreement[List[A]] = read.toList match
        case Nil => Agreement.Undecided
        case xs  => Agreement.Decided(xs)

    override def toString: String =
        lazy val s = s"MultiPaxos(commitIndex: $commitIndex, log: $log)"
        s

object ParallelMultiPaxos:
    def empty[A]: ParallelMultiPaxos[A] = ParallelMultiPaxos[A]()

    given [A]: Lattice[ParallelMultiPaxos[A]] =
        given Lattice[Long] = Math.max
        Lattice.derived

    given [A]: Bottom[ParallelMultiPaxos[A]] = Bottom.provide(empty)
