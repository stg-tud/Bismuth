package rdts.protocols.knowledgeGroups

import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.Paxos.given
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import rdts.protocols.MultipaxosPhase

import scala.collection.immutable.NumericRange
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.precondition

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
    NumericRange(time, log.size.toLong, 1L).view.flatMap(log.get).toSeq

  def read: Seq[A] =
    readSince(0)

  def startLeaderElection(index: Long)(using LocalUid): MultiPaxos[A] =
    precondition(index == 0L || slots.contains(index - 1)) {
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

  def upkeep(using LocalUid, Participants): MultiPaxos[A] = {
    // perform upkeep in open rounds
    val open = NumericRange(log.size.toLong, slots.size.toLong, 1L).view.map(index =>
      (index, slots.getOrElse(index, Paxos()))
    )
    val paxosDeltas = open.map {
      case (index, paxos) => (index, paxos.upkeep())
    }.toMap
    val newPaxosMap = slots.merge(paxosDeltas)

    // move decisions to log
    val newLogEntries = NumericRange(log.size.toLong, slots.size.toLong, 1L).view.flatMap(i =>
      newPaxosMap.get(i).map(p => (i, p))
    ).takeWhile(_._2.result.isDefined).map((i, p) => (i, p.result.get)) // return log until first undecided round

    //val requestsDelta = requests.removeAll(newLogEntries.map(_._2))
    val newState = this.merge(MultiPaxos(slots = newPaxosMap))
    // propose requests for next slots
    val requestElements     = newState.requests.elements
    val requestsWithIndices =
      requestElements.zip(NumericRange(slots.size.toLong, slots.size.toLong + requestElements.size, 1L))
    val newProposals = requestsWithIndices.map((req, i) => newState.proposeIfLeader(i, req))
    val hasRequestedDelta = newProposals.fold(MultiPaxos[A]())((it, delta) => it.merge(delta))

    val requestsDelta =
      if (hasRequestedDelta != MultiPaxos[A]()) then // check if we actually proposed something, which means that we are the leader
        newState.requests.removeAll(requestElements)
      else
        ReplicatedSet.empty[A]

    println(s"$replicaId: I produced the following requestsDelta: $requestsDelta")

    // pack everything together
    MultiPaxos(
      slots = paxosDeltas,
      log = newLogEntries.toMap,
      requests = requestsDelta
    ).merge(hasRequestedDelta)
  }

  def decision(using Participants): Agreement[Seq[A]] = Agreement.Decided(read)

  override def toString: String =
      lazy val s = s"MultiPaxos(commitIndex: ${log.size}, slots: ${slots.size})"
      s

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
