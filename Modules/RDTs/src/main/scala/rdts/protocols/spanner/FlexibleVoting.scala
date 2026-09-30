package rdts.protocols.spanner

import rdts.base.{Bottom, Lattice, ReplicaId}
import rdts.protocols.Quorum.isQuorum
import rdts.protocols.Util.*
import rdts.protocols.Util.Agreement.*
import rdts.protocols.{Participants, Quorum, Vote}

case class FlexibleVoting[A](votes: Set[Vote[A]] = Set.empty[Vote[A]]) {
  // boolean threshold queries
  def hasNotVoted(using replicaId: ReplicaId): Boolean =
    !votes.exists {
      case Vote(r, _) => r == replicaId.uid
    }

  // decision function
  def decision(using Participants, Quorum): Agreement[A] =
    votes
      // count votes
      .groupBy(_.value).map((value, votes) => (value, votes.map(_.voter)))
      // filter by quorum
      .filter((_, votes) => isQuorum(votes))
      // return maximum
      .maxByOption((_, votes) => votes.size) match
        case Some((value, votes)) => Decided(value)
        case None                 => Undecided

  // protocol actions
  def voteFor(value: A)(using replicaId: ReplicaId): FlexibleVoting[A] =
    precondition(hasNotVoted)(
      FlexibleVoting(Set(Vote(replicaId.uid, value)))
    )

  // convenience function to read decision as option
  def result(using Participants, Quorum): Option[A] =
    decision match {
      case Invalid        => None
      case Decided(value) => Some(value)
      case Undecided      => None
    }
}

object FlexibleVoting {
  given [A]: Lattice[FlexibleVoting[A]] = Lattice.derived
  given [A]: Bottom[FlexibleVoting[A]]  =
    Bottom.provide(FlexibleVoting(Set.empty[Vote[A]]))
}
