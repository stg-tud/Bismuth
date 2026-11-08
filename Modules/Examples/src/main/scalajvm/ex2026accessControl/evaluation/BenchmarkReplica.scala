package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import replication.crypto.Hash
import channels.crypto.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.{AntiEntropy, ArdtEventGraph, DeltaValueStore, Replica}

class BenchmarkReplica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy,
    onStateChange: RDT => Unit
) extends Replica[RDT](genesis, privateIdentity, antiEntropyProvider, onStateChange) {

  def currentEventGraph: ArdtEventGraph[RDT]                      = eventGraph
  def currentEventGraph_=(replacement: ArdtEventGraph[RDT]): Unit = synchronized { eventGraph = replacement }

  def currentDeltaValueStore: DeltaValueStore[RDT]                      = deltaValueStore
  def currentDeltaValueStore_=(replacement: DeltaValueStore[RDT]): Unit = synchronized {
    deltaValueStore = replacement
  }

  /** Replaces the materialized state without notifying `onStateChange` */
  def currentMaterializedState: RDT                      = materializedState
  def currentMaterializedState_=(replacement: RDT): Unit = synchronized { materializedState = replacement }

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

object BenchmarkReplica {
  final class Backup[RDT] private[evaluation] (
      private[evaluation] val eventGraph: ArdtEventGraph[RDT],
      private[evaluation] val deltaValueStore: DeltaValueStore[RDT],
      private[evaluation] val materializedState: RDT
  )
}
