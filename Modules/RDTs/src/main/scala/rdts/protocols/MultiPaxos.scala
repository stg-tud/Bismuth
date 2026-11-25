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

/** The leader election of the highest ballot that a replica knows.
  *
  * The merge keeps the election of the higher ballot and merges the votes of equal ballots.
  */
case class NewestElection(ballotNum: BallotNum, votes: LeaderElection)

object NewestElection:
    given Lattice[NewestElection] = (left, right) =>
        val order = Ordering[BallotNum].compare(left.ballotNum, right.ballotNum)
        if order < 0 then right
        else if order > 0 then left
        else NewestElection(left.ballotNum, left.votes.merge(right.votes))

    /** The leader election of the highest ballot of `paxos` that has votes, if any. */
    def of[A](paxos: Paxos[A]): Option[NewestElection] =
      paxos.rounds.filter(_._2.leaderElection.votes.nonEmpty).maxOption.map {
        (ballotNum, round) => NewestElection(ballotNum, round.leaderElection)
      }

    /** Adds the votes of `election` to the round of its ballot in `paxos`. */
    def addTo[A](paxos: Paxos[A], election: Option[NewestElection]): Paxos[A] = election match
        case None                                   => paxos
        case Some(NewestElection(ballotNum, votes)) =>
          val round = PaxosRound[A](leaderElection = votes)
          Paxos(paxos.rounds.updated(ballotNum, paxos.rounds.get(ballotNum).fold(round)(_.merge(round))))

/** Leader elections of Multi-Paxos variants that keep one `Paxos` instance per log slot in a map
  * (`spanner.ParallelMultiPaxos`, `knowledgeGroups.MultiPaxos`).
  *
  * A leader that is elected in ballot `b` in slot `k` may propose in the later slots without a new
  * phase 1. This is safe only if a vote in the election of `b` in slot `k` (phase 1b) is also a
  * promise for all later slots: the voter must not accept proposals of a ballot below `b` in any
  * slot above `k`. The protocol actions of slot `i` therefore act on `view(slots, i)`, which adds
  * the newest leader election of the slots below `i` to slot `i`. Without that, two leaders could
  * get different values chosen in the same slot (see `SlotIndexedMultiPaxosAgreementTest`).
  *
  * The elections stay in the slots where they were started, so a replica that only knows some of
  * the slots still has all of its own promises: they are in slots it knows. Elections only count
  * upwards, so different slots can still have different leaders (a later slot may elect a higher
  * ballot without affecting the earlier ones). Only the newest election below `i` is added, for
  * the same reason as in [[MultiPaxos]].
  */
object SlotElections:

    /** The newest leader election in the slots below `index`. */
    def below[A](slots: Map[Long, Paxos[A]], index: Long): Option[NewestElection] =
      slots.iterator.collect { case (i, paxos) if i < index => NewestElection.of(paxos) }.flatten
        .reduceOption(Lattice.merge)

    /** The Paxos instance on which the protocol actions of slot `index` act. */
    def view[A](slots: Map[Long, Paxos[A]], index: Long): Paxos[A] =
      NewestElection.addTo(slots.getOrElse(index, Paxos[A]()), below(slots, index))

    /** `view(slots, i)` for every `i` in `indices`, computed in one pass over the slots. */
    def views[A](slots: Map[Long, Paxos[A]], indices: Iterable[Long]): Map[Long, Paxos[A]] =
        val wanted                        = indices.toSet
        var below: Option[NewestElection] = None
        val result                        = Map.newBuilder[Long, Paxos[A]]
        for index <- (slots.keySet ++ wanted).toVector.sorted do
            if wanted.contains(index) then
                result += index -> NewestElection.addTo(slots.getOrElse(index, Paxos[A]()), below)
            slots.get(index).flatMap(NewestElection.of).foreach { election =>
              below = Some(below.fold(election)(Lattice.merge(_, election)))
            }
        result.result()

/** Multi-Paxos: one Paxos instance per log slot, with a leader election that is shared by all slots.
  *
  * A vote in the leader election of a ballot (phase 1b) is a promise not to accept proposals of a
  * lower ballot in any slot. The election is therefore kept outside of the `Epoch`: the `Epoch`
  * drops the state of a slot when the next slot is opened, and the promises must survive that.
  * An elected leader can propose in every later slot without a new phase 1.
  *
  * Only the election of the highest known ballot is stored, so the state does not grow with the
  * number of leader elections. The lower elections are not needed: the api functions of `Paxos`
  * act on the highest ballot of `currentPaxos` only (`currentRound`, `currentBallotNum`), and
  * `nextBallotNum` reads only the highest counter. A replica that knows an election of ballot `b`
  * therefore never votes for a lower ballot, whether or not it still stores the lower elections.
  *
  * @param rounds   the proposals of the current slot (`rounds.counter`); the leader elections of
  *                 this `Paxos` are empty
  * @param log      the decided value of each closed slot
  * @param election the leader election of the highest known ballot, for all slots
  */
case class MultiPaxos[A](
    rounds: Epoch[Paxos[A]] = Epoch.empty[Paxos[A]],
    log: Map[Long, A] = Map.empty,
    election: Option[NewestElection] = None
):

    // private helper functions

    /** The Paxos instance of the current slot: its proposals together with the shared leader election. */
    private lazy val currentPaxos: Paxos[A] = NewestElection.addTo(rounds.value, election)

    /** Places a delta of `currentPaxos` in this state: the votes in the leader election count for
      * all slots, the votes on proposals for the current slot. Of the election votes only those of
      * the highest ballot are kept (phase 1b repeats the round of the last accepted value).
      */
    private def embed(delta: Paxos[A]): MultiPaxos[A] =
        val electionVotes = NewestElection.of(delta)
        val proposalVotes = delta.rounds.collect {
          case (ballotNum, PaxosRound(_, proposals)) if proposals.votes.nonEmpty =>
            ballotNum -> PaxosRound(proposals = proposals)
        }
        MultiPaxos(
          rounds = if proposalVotes.isEmpty then Epoch.empty else rounds.write(Paxos(proposalVotes)),
          election = electionVotes
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
            // The next slot starts without proposals. The leader election stays as it is,
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
