package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.Hash
import crypto.channels.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.{AntiEntropy, ArdtEventGraph, Replica, SnapshotAwareReplica}

/** [[BenchmarkReplica]] for [[SnapshotAwareReplica]]. Its [[backup]] and [[restore]] leave the snapshot taken by
  * [[SnapshotAwareReplica.createSnapshot]] alone.
  */
class BenchmarkSnapshotAwareReplica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy,
    onStateChange: RDT => Unit
) extends SnapshotAwareReplica[RDT](genesis, privateIdentity, antiEntropyProvider, onStateChange) {

  def currentEventGraph: ArdtEventGraph[RDT]                      = eventGraph
  def currentEventGraph_=(replacement: ArdtEventGraph[RDT]): Unit = synchronized { eventGraph = replacement }

  /** Backs up the current event graph, delta values and materialized state, to later [[restore]] them. */
  def backup(): BenchmarkReplica.Backup[RDT] = synchronized {
    BenchmarkReplica.Backup(eventGraph, deltaValueStore.copy(), materializedState)
  }

  /** Resets this replica to a [[backup]] previously taken of it, discarding everything received since. The same
    * backup can be restored any number of times.
    */
  def restore(backup: BenchmarkReplica.Backup[RDT]): Unit = synchronized {
    eventGraph = backup.eventGraph
    deltaValueStore = backup.deltaValueStore.copy()
    materializedState = backup.materializedState
    onStateChange(materializedState)
  }
}
