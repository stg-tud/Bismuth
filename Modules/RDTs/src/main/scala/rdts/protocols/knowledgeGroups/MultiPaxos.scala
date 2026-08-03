package rdts.protocols.knowledgeGroups

import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.Paxos.given
import rdts.protocols.Util.Agreement.Undecided
import rdts.protocols.Util.{Agreement, precondition}
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}

import scala.collection.immutable.NumericRange

case class MultiPaxos[A](
    slots: Map[Long, Paxos[A]] = Map.empty[Long, Paxos[A]],
    log: Map[Long, A] = Map.empty[Long, A],
    requests: ReplicatedSet[A] = ReplicatedSet.empty[A]
) {
  // private helper functions
//    private def currentPaxos: Option[Paxos[A]] = slots.get(commitIndex + 1).orElse(slots.get(commitIndex))
//    private def nextSlot(using LocalUid): Long = {
//      val numRounds = slots.size
//      slots.get(numRounds - 1) match {
//        case Some(paxos) => paxos.currentRound match {
//            // reuse slot if there are no votes yet or only votes by somebody else
//            case Some(PaxosRound(_, proposals))
//                if numRounds == 1 && proposals.isEmpty // || proposals.votes.forall(_.voter != replicaId)
//                => numRounds - 1
//            case _ => numRounds
//          }
//        case None => numRounds
//      }
//    }
//
//    // public API
//    def nextDecisionRound = commitIndex + 1
//    def closedRounds      = log
//
//    def leader(using Participants): Option[Uid] = currentPaxos.flatMap(_.currentLeaderElection) match
//        case Some(leaderElection) => leaderElection.result
//        case None                 => None
//
//    // TODO: not sure if we should expose this...
//    def phase(using Participants): MultipaxosPhase =
//      currentPaxos match
//          case Some(paxos) => paxos.currentRound match
//                case Some(PaxosRound(leaderElection, _)) if leaderElection.result.isEmpty =>
//                  MultipaxosPhase.LeaderElection
//                case Some(PaxosRound(leaderElection, proposals))
//                    if leaderElection.result.nonEmpty && proposals.result.isEmpty && proposals.votes.nonEmpty =>
//                  MultipaxosPhase.Voting
//                case Some(PaxosRound(leaderElection, _))
//                    if leaderElection.result.nonEmpty => MultipaxosPhase.Idle
//                case _ => throw new Error("Inconsistent Paxos State")
//          case None if commitIndex == -1 =>
//            MultipaxosPhase.LeaderElection // first round, no previous decision, need to elect leader
//          case None => MultipaxosPhase.Idle // round not yet initialized but previous round was successful

  def request(command: A)(using LocalUid): MultiPaxos[A] =
    MultiPaxos(requests = requests.add(command))

  def readSince(time: Long): Seq[A] =
    NumericRange(time, log.size.toLong, 1L).view.map(log.get).takeWhile(_.isDefined).map(_.get).toSeq

  def read: Seq[A] =
    readSince(0)

  def startLeaderElection(index: Long)(using LocalUid): MultiPaxos[A] =
    precondition(index == 0 || slots.contains(index - 1)) {
      val currentPaxos = slots.getOrElse(index, Paxos[A]())
      MultiPaxos(
        Map(index -> currentPaxos.phase1a)
      ) // start new Paxos round with self proposed as leader
    }

  def proposeIfLeader(index: Long, value: A)(using LocalUid, Participants): MultiPaxos[A] =
    precondition(index == 0L || slots.contains(index - 1)) {
      def openNextSlot = {
        // opens a new slot for the next log entry, either by reusing the old ballot or starting a new one
        slots.get(index - 1).flatMap(_.newestBallotWithLeader) match
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
        slots.getOrElse(index, openNextSlot)

      val paxosVote = paxos.phase2a(value)

      if paxosVote != Paxos() then
          MultiPaxos(
            Map(index -> paxos.merge(paxosVote)) // phase 2a already checks if I am the leader
          )
      else MultiPaxos()
    }

//  def proposeAll(indices: Seq[Long], values: Seq[A])(using LocalUid, Participants): MultiPaxos[A] =
//    val both = indices.zip(values)
//    val a = both.foldLeft(this){case (acc, (i, v)) => acc.merge(acc.proposeIfLeader(i,v))}

  def upkeep(using LocalUid, Participants): MultiPaxos[A] = {
    val openSlots = slots.keySet -- log.keySet
    // perform upkeep in open rounds
    val paxosDeltas = slots.collect {
      case (index, paxos)
          if openSlots.contains(index) && paxos.currentRound.forall(
            !_.proposals.votes.map(_.voter).contains(replicaId)
          ) => (index, paxos.upkeep())
    } // .filter((i, p) => !p.currentRound.contains(PaxosRound()) && !(p == Paxos()))
    val newPaxosMap = slots.merge(paxosDeltas)

    // move decisions to log
    val newLogEntries = newPaxosMap.collect {
      case (index, paxos) if openSlots.contains(index) && paxos.decision != Undecided => (index, paxos.result.get)
    }

    // val requestsDelta = requests.removeAll(newLogEntries.map(_._2))
    // val newState = this.merge(MultiPaxos(slots = newPaxosMap))

    // propose requests for next slots (if we are the leader)
//    val afterRequests = newState.slots.get(newState.slots.size - 1) match { // get newest slot
//      case Some(p @ Paxos(_)) if p.isCurrentLeader =>
//        val requestElements     = newState.requests.elements.toList
//        //println(s"found ${requestElements.size} requests")
//        if requestElements.nonEmpty then {
//          val requestsWithIndices = {
//            requestElements.zip(NumericRange(slots.size.toLong, slots.size.toLong + requestElements.size, 1L))
//          }
//          val firstRequest = newState.proposeIfLeader(requestsWithIndices.head._2, requestsWithIndices.head._1)
//          val hasRequestedDelta = requestsWithIndices.tail.foldLeft(firstRequest) {
//            case (s, (req, i)) => s.merge(s.proposeIfLeader(i, req))
//          }
//
//          val requestsDelta =
//            if hasRequestedDelta.slots.size == requestElements.size
//            then { // check if we actually proposed something, which means that we are the leader
//              newState.requests.removeAll(requestElements)
//            } else
//              ReplicatedSet.empty[A]
//
//          val d = MultiPaxos(requests = requestsDelta).merge(hasRequestedDelta)
//          //println(s"requestsDelta is: $d")
//          d
//        }
//        else MultiPaxos()
//      case _ => MultiPaxos()
//    }

    // pack everything together
    MultiPaxos(
      slots = paxosDeltas,
      log = newLogEntries,
    ) // .merge(afterRequests)
  }

  def decision(using Participants): Agreement[Seq[A]] = Agreement.Decided(read)

//  override def toString: String =
//      lazy val s = s"MultiPaxos(commitIndex: ${log.size}, slots: ${slots.size})"
//      s

}

object MultiPaxos:
    def empty[A]: MultiPaxos[A] = MultiPaxos[A]()

    given [A]: Lattice[MultiPaxos[A]] =
        given Lattice[Map[Long, A]] =
            given Lattice[A] = Lattice.assertEquals
            Lattice.mapLattice
        given Lattice[Long] = Math.max
        Lattice.derived

    given [A]: Bottom[MultiPaxos[A]] = Bottom.provide(empty)
