package rdts.experiments.protocols.raft

import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

import scala.util.Random

case class RaftToken(id: Long, owner: Uid, value: String) derives CanEqual {
  def same(other: RaftToken) = owner == other.owner && value == other.value
}

case class RaftTokens(
    replicaID: Uid,
    tokenAgreement: RaftState[RaftToken],
    want: DeltaBuffer[ReplicatedSet[RaftToken]],
    tokenFreed: DeltaBuffer[ReplicatedSet[RaftToken]],
) {

  given LocalUid = replicaID.convert

  def owned(value: String): List[RaftToken] = {
    val freed  = tokenFreed.state.elements
    val owners = tokenAgreement.values.filter(t => t.value == value && !freed.contains(t))
    val mine   = owners.filter(_.owner == replicaID)
    // return all ownership tokens if this replica owns the oldest one
    if mine.headOption == owners.headOption then mine else Nil
  }

  def isOwned(value: String): Boolean = owned(value).nonEmpty

  def acquire(value: String): RaftTokens = {
    val token = RaftToken(Random.nextLong(), replicaID, value)

    // conditional is only an optimization
    if !(tokenAgreement.values.iterator ++ want.state.elements.iterator).exists(_.same(token)) then {
      copy(want = want.mod(_.add(token)))
    } else this
  }

  def free(value: String): RaftTokens = {
    copy(tokenFreed = tokenFreed.mod(_.addAll(owned(value))))
  }

  def update(): RaftTokens = {
    val generalDuties = tokenAgreement.supportLeader(replicaID).supportProposal(replicaID)

    if tokenAgreement.leader == replicaID then {
      val unwanted = want.mod(_.removeAll(want.state.elements.filter(generalDuties.values.contains)))
      unwanted.state.elements.headOption match {
        case None      => copy(tokenAgreement = generalDuties, want = unwanted)
        case Some(tok) =>
          copy(tokenAgreement = generalDuties.propose(replicaID, tok), want = unwanted)
      }
    } else copy(tokenAgreement = generalDuties)
  }

  def applyWant(state: ReplicatedSet[RaftToken]): RaftTokens = {
    copy(want = want.applyDelta(state))
  }

  def applyFree(state: ReplicatedSet[RaftToken]): RaftTokens = {
    copy(tokenFreed = tokenFreed.applyDelta(state))
  }

  def applyRaft(state: RaftState[RaftToken]): RaftTokens = {
    copy(tokenAgreement = Lattice.merge(tokenAgreement, state))
  }

  def lead(): RaftTokens =
    copy(tokenAgreement = tokenAgreement.becomeCandidate(replicaID))

}

object RaftTokens {
  def init(replicaID: Uid): RaftTokens =
    RaftTokens(
      replicaID,
      RaftState(Set(replicaID)),
      DeltaBuffer(ReplicatedSet.empty[RaftToken]),
      DeltaBuffer(ReplicatedSet.empty[RaftToken])
    )
}
