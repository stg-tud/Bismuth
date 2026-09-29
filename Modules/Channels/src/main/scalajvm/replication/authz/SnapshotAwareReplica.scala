package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.Hash
import crypto.channels.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.ArdtEvent.Payload.DeltaCommitment

/** Adds a very simple snapshot mechanism. Keeps only a single snapshot and only supports snapshotting the latest
  * version.
  */
class SnapshotAwareReplica[RDT: {Lattice, Bottom, Filter, Decompose, JsonValueCodec}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy,
    onStateChange: RDT => Unit
) extends Replica[RDT](genesis, privateIdentity, antiEntropyProvider, onStateChange) {
  private var snapshot: RDT        = Bottom.empty
  private var snapshotVersion: Int = -1

  def createSnapshot(): Unit = synchronized {
    snapshot = materializedState
    snapshotVersion = eventGraph.nextEventIndex - 1
  }

  override protected def applyDelta(delta: RDT, eventIndex: Int): Unit = synchronized {
    materializedState = materializedState.merge(delta)
    if eventIndex < snapshotVersion then snapshot = snapshot.merge(delta)
  }
  override protected def invalidateDeltasAfterRevocation(revocationEventHash: Hash): Boolean = {
    val evGraph                    = eventGraph
    val (revocation, _)            = evGraph.events(revocationEventHash)
    val earliestParentOfRevocation = revocation.parents.map(evGraph.events).minBy(_._2)._2

    if snapshotVersion < earliestParentOfRevocation then {
      snapshotVersion = -1
      snapshot = Bottom.empty
    }

    val newlyRevoked = eventGraph.revocationCache.filter((_, revocations) =>
      revocations.size == 1 && revocations.contains(revocationEventHash)
    ).keySet

    var hasInvalidatedADelta = false
    val toVisit              = scala.collection.mutable.Queue.from(evGraph.heads)
    // TODO: we could instead use the indices and a bitset instead of a hashset with the event hashes
    val visited = scala.collection.mutable.Set.from(evGraph.heads)

    while toVisit.nonEmpty do {
      val nextEvHash          = toVisit.dequeue()
      val (nextEv, nextEvIdx) = evGraph.events(nextEvHash)

      if nextEvIdx > earliestParentOfRevocation then {
        nextEv match {
          case ArdtEvent(DeltaCommitment(commitmentHash), _, parents, _, auth) =>
            if newlyRevoked.contains(auth) && !eventGraph.causallyBefore(nextEvHash, revocationEventHash)
            then hasInvalidatedADelta |= deltaValueStore.remove(commitmentHash).nonEmpty
          case _ =>
        }
        val parents = nextEv.parents.diff(visited)
        toVisit.enqueueAll(parents)
        visited.addAll(parents)
      }
    }

    hasInvalidatedADelta
  }

  override protected def rematerialize(): Unit = synchronized {
    if snapshotVersion == -1 then
        materializedState = deltaValueStore.merged
        return

    val evGraph = eventGraph
    val toVisit = scala.collection.mutable.Queue.from(evGraph.heads)
    // TODO: we could instead use the indices and a bitset instead of a hashset with the event hashes
    val visited             = scala.collection.mutable.Set.from(evGraph.heads)
    var rematerializedState = snapshot

    while toVisit.nonEmpty do {
      val nextEvHash          = toVisit.dequeue()
      val (nextEv, nextEvIdx) = evGraph.events(nextEvHash)

      if nextEvIdx > snapshotVersion then {
        nextEv match {
          case ArdtEvent(DeltaCommitment(commitmentHash), _, _, _, _) =>
            deltaValueStore.get(commitmentHash).foreach { case (delta, _) =>
              rematerializedState = rematerializedState.merge(delta)
            }
          case _ =>
        }
        val parents = nextEv.parents.diff(visited)
        toVisit.enqueueAll(parents)
        visited.addAll(parents)
      }
    }

    materializedState = rematerializedState
  }

}
