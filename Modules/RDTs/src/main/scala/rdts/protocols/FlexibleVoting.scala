package rdts.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Participants
import rdts.protocols.Participants.participants
import Util.*
import Util.Agreement.*
import rdts.protocols.Quorum.isQuorum

case class FlexibleVoting[A](votes: Set[Vote[A]] = Set.empty[Vote[A]]) {
  // boolean threshold queries
  def hasNotVoted(using LocalUid): Boolean =
    !votes.exists {
      case Vote(r, _) => r == replicaId
    }

  // decision function
  def decision(using Participants, Quorum): Agreement[A] =
    votes
      // count votes
      .groupBy(_.value).map((value, votes) => (value, votes.map(_.voter)))
      // filter by quorum
      .filter((_, votes) => isQuorum(votes))
      // return maximum
      .maxByOption((value, votes) => votes.size) match
        case Some((value, votes)) => Decided(value)
        case None                 => Undecided

  // protocol actions
  def voteFor(value: A)(using LocalUid): FlexibleVoting[A] =
    precondition(hasNotVoted)(
      FlexibleVoting(Set(Vote(replicaId, value)))
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
