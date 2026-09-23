package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.Hash
import crypto.channels.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.{AntiEntropy, ArdtEventGraph, DeltaValueStore, Replica}

/** [[Replica]] exposing its internal state for reading and writing, so that benchmarks can manually put it back
  * into the state it had before an invocation, and thus repeatedly measure an operation on the same state.
  */
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

  /** Captures the current event graph, delta values and materialized state, to later [[restore]] them. */
  def snapshot(): BenchmarkReplica.Snapshot[RDT] = synchronized {
    BenchmarkReplica.Snapshot(eventGraph, deltaValueStore.copy(), materializedState)
  }

  /** Resets this replica to a [[snapshot]] previously taken from it, discarding everything received since. The
    * same snapshot can be restored any number of times.
    */
  def restore(snapshot: BenchmarkReplica.Snapshot[RDT]): Unit = synchronized {
    eventGraph = snapshot.eventGraph
    deltaValueStore = snapshot.deltaValueStore.copy()
    materializedState = snapshot.materializedState
    onStateChange(materializedState)
  }
}

object BenchmarkReplica {

  /** The state of a [[BenchmarkReplica]] at some point in time, see [[BenchmarkReplica.snapshot]] */
  final class Snapshot[RDT] private[BenchmarkReplica] (
      private[BenchmarkReplica] val eventGraph: ArdtEventGraph[RDT],
      private[BenchmarkReplica] val deltaValueStore: DeltaValueStore[RDT],
      private[BenchmarkReplica] val materializedState: RDT
  )
}
