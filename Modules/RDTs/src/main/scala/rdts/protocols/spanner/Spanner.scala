package rdts.protocols.spanner

import rdts.base.Uid
import rdts.base.LocalUid
import rdts.protocols.Util.precondition
import rdts.base.LocalUid.replicaId
import rdts.base.Bottom
import rdts.protocols.Quorum.FullQuorum
import rdts.protocols.Util.Agreement
import rdts.protocols.MultiPaxos
import rdts.protocols.TwoPhaseCommit
import rdts.protocols.Participants

enum twoPCMessages:
    case Prepare(transactionID: Uid, valid: Boolean)
    case Commit(transactionID: Uid)
    case Abort(transactionID: Uid)

case class Spanner[A](
    replicas: Set[Uid] = Set.empty[Uid],
    paxosPartitions: Map[Uid, MultiPaxos[twoPCMessages]] = Map.empty[Uid, MultiPaxos[twoPCMessages]],
    partitionMembers: Map[Uid, Set[Uid]] = Map.empty[Uid, Set[Uid]],
    transactions: Map[Uid, TwoPhaseCommit[A]] = Map.empty[Uid, TwoPhaseCommit[A]]
) {
  // step 1: initialize a new TwoPhaseCommit transaction. This can only be done by leaders
  def startTransaction(localPartitionId: Uid, t: A)(using LocalUid): Spanner[A] =
    // todo: this is not stable... fix
    precondition(
      paxosPartitions.contains(localPartitionId) &&
      partitionMembers.contains(localPartitionId) &&
      // can only start committing if I am the leader of my partition
      paxosPartitions(localPartitionId).leader(
        using Participants(partitionMembers(localPartitionId))
      ).contains(replicaId)
    ) {

      // initiate new TwoPhaseCommit
      val init2PC            = TwoPhaseCommit(coordinator = Some(localPartitionId), transaction = Some(t))
      val proposeTransaction =
        init2PC.proposeTransaction(using LocalUid(localPartitionId))

      Spanner(transactions = Map(Uid.gen() -> init2PC.merge(proposeTransaction)))
    }

  // step 2 & 3: validate per partition if the transaction should be committed. Then persist upcoming 2PC vote in the partition's paxos log
  def validate2PC(localPartitionId: Uid, transactionID: Uid, valid: Boolean)(using l: LocalUid): Spanner[A] =
    precondition(
      paxosPartitions.contains(localPartitionId) &&
      partitionMembers.contains(localPartitionId) &&
      transactions.contains(transactionID)
    ) {
      val newPaxos = paxosPartitions(localPartitionId)
        .proposeIfLeader(
          twoPCMessages.Prepare(transactionID, valid)
        )(using l, Participants(partitionMembers(localPartitionId)))

      Spanner(paxosPartitions = Map(localPartitionId -> newPaxos))
    }

  // step 4: after logging the 2PC vote, send the vote in the commit phase
  def voteIn2PC(partitionId: Uid, transactionID: Uid, valid: Boolean)(using l: LocalUid): Spanner[A] =
    precondition(
      paxosPartitions.contains(partitionId) &&
      partitionMembers.contains(partitionId) &&
      transactions.contains(transactionID)
    ) {
      val transaction = transactions(transactionID)
      val vote        = paxosPartitions(partitionId).read.find {
        case twoPCMessages.Prepare(id, _) =>
          transactionID == id
        case _ => false
      }
      vote match
          case Some(twoPCMessages.Prepare(_, valid))
              // only the leader should vote in 2PC
              if paxosPartitions(partitionId).leader(using Participants(partitionMembers(partitionId))).contains(
                replicaId
              ) =>
            val twoPC =
              transactions(transactionID).prepare(valid)(using LocalUid(partitionId))
            Spanner(transactions = Map(transactionID -> twoPC))
          case _ => Spanner()
    }

  // step 5: log overall 2PC decision in the coordinator's paxos log
  // seems unnecessary with PRDT
  def log2PCDecisionCoordinator(partitionId: Uid, transactionID: Uid)(using l: LocalUid): Spanner[A] =
    precondition(
      paxosPartitions.contains(partitionId) &&
      partitionMembers.contains(partitionId) &&
      transactions.contains(transactionID) &&
      transactions(transactionID).coordinator.contains(partitionId) && // we are the coordinator
      transactions(transactionID).prepare.decision(using
        Participants(paxosPartitions.keySet),
        FullQuorum
      ) != Agreement.Undecided
    ) {
      val transaction = transactions(transactionID)
      val result      = transaction.prepare.decision(using Participants(paxosPartitions.keySet), FullQuorum) match
          case Agreement.Decided(true) => twoPCMessages.Commit(transactionID)
          case _                       => twoPCMessages.Abort(transactionID)

      val newPaxos = paxosPartitions(partitionId)
        .proposeIfLeader(
          result
        )(using l, Participants(partitionMembers(partitionId)))

      Spanner(paxosPartitions = Map(partitionId -> newPaxos))
    }

  // step 6: return decision to the client

  // step 7: relay 2PC commit/abort decision to other participants
  // again, seems unnecessary with PRDT. This is like starting phase 2 in 2PC as the coordinator which is unnecessary given eventual common knowledge and unsupported in implementation version of 2PC

  // step 8: every participant logs the 2PC decision in their paxos log
  // also seem unnecessary?
  def log2PCDecisionParticipant(partitionId: Uid, transactionID: Uid)(using l: LocalUid): Spanner[A] = {
    precondition(
      paxosPartitions.contains(partitionId) &&
      partitionMembers.contains(partitionId) &&
      transactions.contains(transactionID) &&
      !transactions(transactionID).coordinator.contains(partitionId) && // we are a participant partition
      transactions(transactionID).prepare.decision(using
        Participants(paxosPartitions.keySet),
        FullQuorum
      ) != Agreement.Undecided
    ) {
      val transaction = transactions(transactionID)
      val result      = transaction.prepare.decision(using Participants(paxosPartitions.keySet), FullQuorum) match
          case Agreement.Decided(true) => twoPCMessages.Commit(transactionID)
          case _                       => twoPCMessages.Abort(transactionID)

      val newPaxos = paxosPartitions(partitionId)
        .proposeIfLeader(
          result
        )(using l, Participants(partitionMembers(partitionId)))

      Spanner(paxosPartitions = Map(partitionId -> newPaxos))
    }
  }

  // step 9: release locks

  // step 10: reply with 2PC ack
  // TODO: should this only be done by the leader of each partition?
  def ack2PC(partitionId: Uid, transactionID: Uid): Spanner[A] =
    precondition(paxosPartitions.contains(partitionId) && transactions.contains(transactionID)) {
      val transaction     = transactions(transactionID)
      val acknowledgement =
        transaction.acknowledge(using LocalUid(partitionId))(using Participants(paxosPartitions.keySet))

      Spanner(transactions = Map(transactionID -> acknowledgement))
    }

}

object Spanner {
  given [A]: Bottom[Spanner[A]] = Bottom.derived
}
