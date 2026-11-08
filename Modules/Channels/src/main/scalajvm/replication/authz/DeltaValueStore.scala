package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import replication.crypto.Commitment.RevealedValue
import rdts.base.{Bottom, Lattice}

import scala.collection.immutable.IntMap

/** Stores the delta values (and the witnesses of their commitments) by the index of their event in the
  * [[ArdtEventGraph]], i.e., [[ArdtEventGraph.events]]'s `Int`. Immutable.
  */
final case class DeltaValueStore[Delta](
    backingStore: IntMap[(delta: Delta, witness: Array[Byte])] = IntMap.empty[(delta: Delta, witness: Array[Byte])]
) {

  /** Assumes that the commitment of the event at `eventIndex` is correct */
  def put(eventIndex: Int, value: Delta, witness: Array[Byte]): DeltaValueStore[Delta] =
    DeltaValueStore(backingStore.updated(eventIndex, (value, witness)))

  def removedAll(eventIndices: Iterable[Int]): DeltaValueStore[Delta] =
    DeltaValueStore(backingStore.removedAll(eventIndices))

  def contains(eventIndex: Int): Boolean = backingStore.contains(eventIndex)

  def indices: Set[Int] = backingStore.keySet

  def get(eventIndex: Int): Option[(delta: Delta, witness: Array[Byte])] = backingStore.get(eventIndex)

  def getRevealedValue(eventIndex: Int)(using JsonValueCodec[Delta]): Option[RevealedValue] =
    backingStore.get(eventIndex).map(stored => RevealedValue(writeToArray(stored.delta), stored.witness))

  def merged(using Lattice[Delta], Bottom[Delta]): Delta =
    backingStore.values.foldLeft(Bottom.empty)((acc, deltaValue) => acc.merge(deltaValue.delta))
}
