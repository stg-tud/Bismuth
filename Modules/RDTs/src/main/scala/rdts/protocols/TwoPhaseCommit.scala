package rdts.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Quorum.FullQuorum
import rdts.protocols.Util.{Agreement, precondition}

case class TwoPhaseCommit[A](
    coordinator: Option[Uid] = None, // determined at construction
    transaction: Option[A] = None,   // determined at construction
    prepare: FlexibleVoting[Boolean] = FlexibleVoting(),
    commit: FlexibleVoting[Boolean] = FlexibleVoting()
):
    // as the coordinator, propose a transaction
    def proposeTransaction(using LocalUid, Participants): TwoPhaseCommit[A] =
      precondition(coordinator == Some(replicaId)) {
        TwoPhaseCommit(prepare = prepare.voteFor(true))
      }

    // as a participant, vote for commit in the request phase
    def prepare(using LocalUid, Participants): TwoPhaseCommit[A] =
      precondition(transaction.isDefined && prepare.votes.nonEmpty) {
        TwoPhaseCommit(prepare = prepare.voteFor(true))
      }

    // as a participant, vote for abort in the request phase
    def abort(using LocalUid, Participants): TwoPhaseCommit[A] =
      precondition(transaction.isDefined) {
        TwoPhaseCommit(prepare = prepare.voteFor(false))
      }

    // everybody,
    // check if request phase was accepted by everyone
    // commit the transaction and send ack to the others
    def acknowledge(using LocalUid)(using p: Participants): TwoPhaseCommit[A] =
      // check if there is a transaction and everybody has voted
      precondition(transaction.isDefined && prepare.decision(using p, FullQuorum) != Agreement.Undecided) {
        prepare.decision(using p, FullQuorum) match
            case Agreement.Decided(true) => TwoPhaseCommit(commit = commit.voteFor(true))
            case _                       => TwoPhaseCommit(commit = commit.voteFor(false))
      }

    // returns if the transaction was committed successfully or not
    def decision(using p: Participants): Agreement[Boolean] =
      // return false if there are any votes for false, otherwise return the commit decision
      if commit.votes.filter(_.value == false).nonEmpty then
          Agreement.Decided(false)
      else
          commit.decision(using p, FullQuorum)

object TwoPhaseCommit:
    given [A]: Lattice[TwoPhaseCommit[A]] =
        given Lattice[Uid] = Lattice.assertEquals
        given Lattice[A]   = Lattice.assertEquals
        Lattice.derived

    given [A]: Bottom[TwoPhaseCommit[A]] = Bottom.derived
