package rdts.protocols.paper

import Util.*

case class Vote[A](voter: Uid, value: A)

case class Voting[A](votes: Set[Vote[A]]) {
  // boolean threshold queries
  def hasNotVoted: Boolean = //§\label{line:voting-hasvoted}
    !votes.exists {
         case Vote(r, _) => r == replicaId
    }

  // decision function
  def decision: Agreement[A] = //§\label{line:voting-decision}
    if hasDuplicateVotes() then Invalid
    else getLeadingValue() match
      case Some(value, count) if count >= majority => Decided(value)
      case _ => Undecided

  def hasDuplicateVotes: Boolean =
    votes.groupBy(_.voter).values.filter(_.size > 1).nonEmpty
  def getLeadingValue(): Option[(A, Int)] =
    votes.groupBy(_.value)
      .map((value,vts) => (value, vts.size)).maxByOption(_._2)

  // protocol actions
  def voteFor(value: A): Voting[A] = //§\label{line:voting-votefor}
    updateIf(hasNotVoted)( //§\label{line:updateif}
      Voting(Set(Vote(replicaId, value))) //§\label{line:voting-voteforreturn}
    )
}
