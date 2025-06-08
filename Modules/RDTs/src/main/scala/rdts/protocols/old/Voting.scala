package rdts.protocols.old

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{Epoch, LastWriterWins, ReplicatedSet}

case class Vote(leader: Uid, voter: Uid)

case class Voting(rounds: Epoch[ReplicatedSet[Vote]], numParticipants: LastWriterWins[Int]) {
  def threshold: Int = numParticipants.value / 2 + 1

  def isOwner(using LocalUid): Boolean =
    val (id, count) = leadingCount
    id == replicaId && count >= threshold

  def request(using LocalUid): Voting =
    if !rounds.value.isEmpty then Voting.unchanged
    else voteFor(replicaId)

  def release(using LocalUid): Voting =
    if !isOwner
    then Voting.unchanged
    else Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty), numParticipants)

  def upkeep(using LocalUid): Voting =
    val (id, count) = leadingCount
    if checkIfMajorityPossible(count)
    then voteFor(id)
    else forceRelease

  def forceRelease: Voting =
    Voting(Epoch(rounds.counter + 1, ReplicatedSet.empty), numParticipants)

  def voteFor(uid: Uid)(using LocalUid): Voting =
    if rounds.value.elements.exists { case Vote(_, voter) => voter == replicaId }
    then Voting.unchanged // already voted!
    else
      val newVote = rounds.value.add(Vote(uid, replicaId))
      Voting(rounds.write(newVote), numParticipants)

  def checkIfMajorityPossible(count: Int): Boolean =
    val totalVotes     = rounds.value.elements.size
    val remainingVotes = numParticipants.value - totalVotes
    (count + remainingVotes) > threshold

  def leadingCount(using id: LocalUid): (Uid, Int) =
    val votes: Set[Vote]       = rounds.value.elements
    val grouped: Map[Uid, Int] = votes.groupBy(_.leader).map((o, elems) => (o, elems.size))
    if grouped.isEmpty
    then (replicaId, 0)
    else grouped.maxBy((o, size) => size)
  // .maxBy((o, size) => size)
}

object Voting {
  given Bottom[Int] with
    def empty = 0
  val unchanged: Voting = Voting(Epoch.empty, LastWriterWins.empty[Int])

  given Lattice[Voting] = Lattice.derived

}
