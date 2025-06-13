package rdts.protocols.paper

import Util.*
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.base.LocalUid.replicaId
import Util.Agreement.*
import rdts.protocols.Participants.participants
import rdts.protocols.Participants

case class Vote[A](voter: Uid, value: A)

case class Voting[A](votes: Set[Vote[A]]) {
  // boolean threshold queries
  def hasNotVoted(using LocalUid): Boolean =
    !votes.exists {
      case Vote(r, _) => r == replicaId
    }

  // decision function
  def decision(using Participants): Agreement[A] =
    if hasDuplicateVotes() then Invalid
    else
      getLeadingValue() match
        case Some(value, count) if count >= majority => Decided(value)
        case _                                       => Undecided

  // helper functions
  def majority(using Participants) =
    participants.size / 2 + 1
  def hasDuplicateVotes(): Boolean =
    votes.groupBy(_.voter).values.filter(_.size > 1).nonEmpty
  def getLeadingValue(): Option[(A, Int)] =
    votes.groupBy(_.value)
      .map((value, vts) => (value, vts.size)).maxByOption(_._2)

  // protocol actions
  def voteFor(value: A)(using LocalUid): Voting[A] =
    updateIf(hasNotVoted)(
      Voting(Set(Vote(replicaId, value)))
    )

  // convenience function to read decision as option
  def result(using Participants): Option[A] =
    decision match {
      case Invalid        => None
      case Decided(value) => Some(value)
      case Undecided      => None
    }
}

object Voting {
  given [A]: Lattice[Voting[A]] = Lattice.derived
  given [A]: Bottom[Voting[A]]  =
    Bottom.provide(Voting(Set.empty[Vote[A]]))
}
