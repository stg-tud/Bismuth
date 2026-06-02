package rdts.protocols.spanner

import rdts.base.Uid
import rdts.protocols.MultiPaxos
import rdts.base.LocalUid
import rdts.protocols.Participants
import rdts.protocols.Util.precondition
import rdts.base.LocalUid.replicaId
import rdts.base.Bottom
import rdts.protocols.TwoPhaseCommit
import rdts.protocols.Quorum.FullQuorum
import rdts.protocols.Util.Agreement
import rdts.base.Lattice
import rdts.protocols.Util.Agreement.Decided
import rdts.protocols.MultipaxosPhase

case class SimpSpan[A](
    replicas: Set[Uid] = Set.empty[Uid],
    paxosPartitions: Map[Uid, MultiPaxos[twoPCMessages]] = Map.empty[Uid, MultiPaxos[twoPCMessages]],
    partitionMembers: Map[Uid, Set[Uid]] = Map.empty[Uid, Set[Uid]],
    transactions: Map[Uid, TwoPhaseCommit[A]] = Map.empty[Uid, TwoPhaseCommit[A]]
) {

  // helper functions
  def localPartitionId(using LocalUid): Option[Uid] = partitionMembers.find(_._2.contains(replicaId)).map(_._1)

  // step 1: initialize a new transaction. This can only be done by leaders
  def startTransaction(localPartitionId: Uid, t: A)(using LocalUid): SimpSpan[A] =
    // todo: this is not stable... fix
    precondition(
      paxosPartitions.contains(localPartitionId) &&
      partitionMembers.contains(localPartitionId) &&
      // can only start committing if I am the leader of my partition
      paxosPartitions(localPartitionId).leader(
        using Participants(partitionMembers(localPartitionId))
      ).contains(replicaId)
    ) {

      // initiate new transaction
      SimpSpan(
        transactions = Map(Uid.gen() -> TwoPhaseCommit(coordinator = Some(localPartitionId), transaction = Some(t)))
      )
    }

  // step 2: validate per partition if the transaction should be committed. Then persist upcoming 2PC vote in the partition's paxos log
  def validate2PC(localPartitionId: Uid, transactionID: Uid, valid: Boolean)(using l: LocalUid): SimpSpan[A] =
    precondition(
      paxosPartitions.contains(localPartitionId) &&
      partitionMembers.contains(localPartitionId) &&
      partitionMembers(localPartitionId).contains(replicaId) &&
      transactions.contains(transactionID)
    ) {
      val newPaxos = paxosPartitions(localPartitionId)
        .proposeIfLeader(
          twoPCMessages.Prepare(transactionID, valid)
        )(using l, Participants(partitionMembers(localPartitionId)))

      SimpSpan(paxosPartitions = Map(localPartitionId -> newPaxos))
    }

  // step 3: acknowledge 2PC outcome in each partition's paxos log
  def acknowledge2PC(localPartitionId: Uid, transactionID: Uid)(using l: LocalUid): SimpSpan[A] =
    precondition(
      paxosPartitions.contains(localPartitionId) &&
      partitionMembers.contains(localPartitionId) &&
      partitionMembers(localPartitionId).contains(replicaId) &&
      transactions.contains(transactionID) &&
      (transactions(transactionID).prepare.decision(using
        Participants(paxosPartitions.keySet),
        FullQuorum
      ) == Agreement.Decided(true) ||
      transactions(transactionID).prepare.votes.map(_.value).contains(false)) // if any partition has voted false, abort
    ) {
      val transaction = transactions(transactionID)
      // check if transaction should be committed or aborted
      val vote =
        if transaction.prepare.decision(using Participants(paxosPartitions.keySet), FullQuorum) != Agreement.Decided(
              true
            )
        then twoPCMessages.Abort(transactionID)
        else
            twoPCMessages.Commit(transactionID)

      SimpSpan(paxosPartitions =
        Map(
          localPartitionId -> paxosPartitions(localPartitionId).proposeIfLeader(
            vote
          )(using l, Participants(partitionMembers(localPartitionId)))
        )
      )
    }

  def upkeep(using l: LocalUid): SimpSpan[A] = {
    localPartitionId.map { partitionId =>
      // upkeep local multi-paxos instance
      val paxosDelta = paxosPartitions(partitionId).upkeep(using l, Participants(partitionMembers(partitionId)))

      // filter empty paxos deltas
      val newPaxosDelta = {
        if paxosDelta == Bottom[MultiPaxos[twoPCMessages]].empty then Map()
        else Map(partitionId -> paxosDelta)
      }

      val paxosUpkept = Lattice.merge(paxosPartitions, newPaxosDelta)

      // acknowledge current transaction in the partition's paxos log
      // only do this if the partition is idle (i.e., all transactions are decided)
      val ackDelta: SimpSpan[A] = {
        if paxosUpkept(partitionId).phase(using Participants(partitionMembers(partitionId))) == MultipaxosPhase.Idle
        then {
          // find first unacknowledged transaction
          val firstUnacknowledged =
            transactions.find {
              case (_, twoPC) =>
                given Participants(paxosPartitions.keySet)
                twoPC.prepareDecision != Agreement.Undecided &&    // find transaction that is decided
                !twoPC.commit.votes.exists(_.voter == partitionId) // but not yet acknowledged by this partition
            }

          // acknowledge transaction in partition's paxos log
          firstUnacknowledged.map { (transactionID, _) =>
            acknowledge2PC(partitionId, transactionID)
          }
        } else None
      }.getOrElse(Bottom[SimpSpan[A]].empty)

      // transfer step 2 & 3 votes to 2PC states
      val newTransactionsDelta = {
        val paxos = paxosUpkept(partitionId)
        // iterate over this partition's paxos log and vote in corresponding 2PC states
        // TODO: Instead of going through the whole log, go through the new parts of the log
        // TODO: should this be a protocol action too?
        paxos.read.foldLeft(Map.empty[Uid, TwoPhaseCommit[A]]) {
          case (acc, twoPCMessages.Prepare(transactionID, valid)) =>
            val delta = transactions(transactionID).prepare(valid)(using LocalUid(partitionId))
            if delta != Bottom[TwoPhaseCommit[A]].empty then
                acc + (transactionID -> delta)
            else
                acc
          case (acc, twoPCMessages.Commit(transactionID)) =>
            val delta =
              transactions(transactionID).acknowledge(using
                LocalUid(partitionId),
                Participants(paxosPartitions.keySet)
              )

            if delta != Bottom[TwoPhaseCommit[A]].empty then
                acc + (transactionID -> delta)
            else
                acc
          case (acc, twoPCMessages.Abort(transactionID)) =>
            val delta =
              transactions(transactionID).acknowledge(using
                LocalUid(partitionId),
                Participants(paxosPartitions.keySet)
              )

            if delta != Bottom[TwoPhaseCommit[A]].empty then
                acc + (transactionID -> delta)
            else
                acc
        }
      }

      // compile everything into one delta
      Lattice.merge(
        ackDelta,
        SimpSpan(
          paxosPartitions = newPaxosDelta,
          transactions = newTransactionsDelta
        )
      )
    }.getOrElse(Bottom[SimpSpan[A]].empty)
  }

  // reports which transactions will/have been committed or aborted
  def decision: Agreement[Map[Uid, Boolean]] = {
    // as an optimization, we can report transactions as decided once all partitions have agreed on them, even when that result was not yet acknowledged by all partitions
    Agreement.Decided(transactions.collect {
      case (transactionID, twoPC)
          if
          !twoPC.prepare.votes.forall(_.value) || // can return abort if anybody voted false
          twoPC.prepare.decision(using Participants(paxosPartitions.keySet), FullQuorum) == Decided(
            true
          ) => // can return accept if everybody voted true
        (
          transactionID,
          twoPC.prepare.decision(using Participants(paxosPartitions.keySet), FullQuorum) == Agreement.Decided(true)
        )
    })
  }
}

object SimpSpan:
    given [A]: Bottom[SimpSpan[A]]  = Bottom.provide(SimpSpan())
    given [A]: Lattice[SimpSpan[A]] = Lattice.derived
