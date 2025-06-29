package rdts.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.Epoch
import Participants.participants

case class Vote[A](value: A, voter: Uid)

case class Voting[A](votes: Set[Vote[A]] = Set.empty[Vote[A]]) {
  def threshold(using Participants): Int = participants.size / 2 + 1

  def result(using Participants): Option[A] =
    leadingCount match
      case Some((v, count)) if count >= threshold => Some(v)
      case _                                      => None

  def voteFor(v: A)(using LocalUid, Participants): Voting[A] =
    if !participants.contains(replicaId) || votes.exists { case Vote(_, voter) => voter == replicaId }
    then Voting.unchanged // already voted!
    else
      Voting(Set(Vote(v, replicaId)))

  def leadingCount: Option[(A, Int)] =
    val grouped: Map[A, Int] = votes.groupBy(_.value).map((value, vts) => (value, vts.size))
    grouped.maxByOption((_, size) => size)
}

object Voting {
  given lattice[A]: Lattice[Voting[A]] = Lattice.derived

  given bottom[A]: Bottom[Voting[A]] with
    override def empty: Voting[A] = unchanged

  def unchanged[A]: Voting[A] = Voting(Set.empty)
}

case class MultiRoundVoting[A](rounds: Epoch[Voting[A]]):
  def release: MultiRoundVoting[A] =
    MultiRoundVoting(Epoch(rounds.counter + 1, Voting.unchanged))

  def upkeep(using LocalUid, Participants): MultiRoundVoting[A] =
    rounds.value.leadingCount match
      case Some(value, count) if checkIfMajorityPossible => voteFor(value)
      case Some(_) => release                    // we have a leading proposal but majority is not possible anymore
      case None    => MultiRoundVoting.unchanged // no change yet

  def checkIfMajorityPossible(using Participants): Boolean =
    val totalVotes     = rounds.value.votes.size
    val remainingVotes = participants.size - totalVotes
    val possible       = rounds.value.leadingCount.map((_, count) => (count + remainingVotes) >= rounds.value.threshold)
    possible.getOrElse(true) // if there is no leading vote, majority is always possible

  // api
  def voteFor(c: A)(using LocalUid, Participants): MultiRoundVoting[A] =
    MultiRoundVoting(Epoch(rounds.counter, rounds.value.voteFor(c)))

  def result(using Participants): Option[A] =
    rounds.value.result

object MultiRoundVoting {
  def unchanged[A]: MultiRoundVoting[A]          = MultiRoundVoting(Epoch.empty[Voting[A]])
  given lattice[A]: Lattice[MultiRoundVoting[A]] = Lattice.derived
}

case class BallotNum(uid: Uid, counter: Long)

object BallotNum:
  given Ordering[BallotNum] with
    override def compare(x: BallotNum, y: BallotNum): Int =
      if x.counter > y.counter then 1
      else if x.counter < y.counter then -1
      else Ordering[Uid].compare(x.uid, y.uid)
