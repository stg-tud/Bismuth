package rdts.protocols

import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, ReplicaId, Uid}
import rdts.datatypes.Epoch
import rdts.protocols.Participants
import rdts.protocols.Paxos.given
import rdts.time.Time

import scala.collection.immutable.NumericRange

enum MultipaxosPhase:
    case LeaderElection
    case Voting
    case Idle // Leader is elected but no votes have been cast yet

/** Multi-Paxos: one Paxos instance per log slot, with leader elections that are shared by all slots.
  *
  * A vote in the leader election of a ballot (phase 1b) is a promise not to accept proposals of a
  * lower ballot in any slot. The elections are therefore kept outside of the `Epoch`: the `Epoch`
  * drops the state of a slot when the next slot is opened, and the promises must survive that.
  * An elected leader can propose in every later slot without a new phase 1.
  *
  * @param rounds    the proposals of the current slot (`rounds.counter`); the leader elections of
  *                  this `Paxos` are empty
  * @param log       the decided value of each closed slot
  * @param elections the leader election of each ballot, for all slots
  */
case class MultiPaxos[A](
    rounds: Epoch[Paxos[A]] = Epoch.empty[Paxos[A]],
    log: Map[Long, A] = Map.empty,
    elections: Map[BallotNum, LeaderElection] = Map.empty
):

    // private helper functions

    /** The Paxos instance of the current slot: its proposals together with the shared leader elections. */
    private lazy val currentPaxos: Paxos[A] =
      Paxos(elections.view.mapValues(election => PaxosRound[A](leaderElection = election)).toMap).merge(rounds.value)

    /** Places a delta of `currentPaxos` in this state: the votes in leader elections count for all
      * slots, the votes on proposals for the current slot.
      */
    private def embed(delta: Paxos[A]): MultiPaxos[A] =
        val electionVotes = delta.rounds.collect {
          case (ballotNum, PaxosRound(leaderElection, _)) if leaderElection.votes.nonEmpty =>
            ballotNum -> leaderElection
        }
        val proposalVotes = delta.rounds.collect {
          case (ballotNum, PaxosRound(_, proposals)) if proposals.votes.nonEmpty =>
            ballotNum -> PaxosRound(proposals = proposals)
        }
        MultiPaxos(
          rounds = if proposalVotes.isEmpty then Epoch.empty else rounds.write(Paxos(proposalVotes)),
          elections = electionVotes
        )

    def closedRounds: Map[Long, A] = log
    def nextDecisionRound          = rounds.counter

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

    def startLeaderElection(using ReplicaId): MultiPaxos[A] =
      embed(currentPaxos.phase1a) // start new Paxos round with self proposed as leader

    def proposeIfLeader(value: A)(using ReplicaId, Participants): MultiPaxos[A] =
      embed(currentPaxos.phase2a(value)) // phase 2a already checks if I am the leader

    def upkeep(using ReplicaId, Participants): MultiPaxos[A] = {
      // perform upkeep in Paxos
      val deltaPaxos = currentPaxos.upkeep()
      val delta      = embed(deltaPaxos)

      currentPaxos.merge(deltaPaxos).result match
          case Some(decision) =>
            // The current slot is decided: append its value to the log and open the next slot.
            // The next slot starts without proposals. The leader elections stay as they are,
            // so the current leader can propose in the next slot at once.
            delta.copy(
              rounds = rounds.epocheWrite(Paxos[A]()),
              log = Map(rounds.counter -> decision)
            )
          case None =>
            // nothing to do, return upkeep result (an empty Paxos delta gives the empty MultiPaxos)
            delta
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
