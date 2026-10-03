package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.Commitment.RevealedValue
import rdts.base.{Bottom, Lattice}

import scala.collection.immutable.IntMap

/** Stores the delta values (and the witnesses of their commitments) by the index of their event in the
  * [[ArdtEventGraph]], i.e., [[ArdtEventGraph.events]]'s `Int`.
  */
class DeltaValueStore[Delta: JsonValueCodec] {
  // Stores delta and witness (salt of commitment)
  @volatile private var backingStore: IntMap[(delta: Delta, witness: Array[Byte])] = IntMap.empty

  /** Assumes that the commitment of the event at `eventIndex` is correct */
  def put(eventIndex: Int, value: Delta, witness: Array[Byte]): Unit = synchronized {
    backingStore = backingStore.updated(eventIndex, (value, witness))
  }

  def remove(eventIndex: Int): Option[(delta: Delta, witness: Array[Byte])] = synchronized {
    val removed = backingStore.get(eventIndex)
    backingStore = backingStore.removed(eventIndex)
    removed
  }

  def get(eventIndex: Int): Option[(delta: Delta, witness: Array[Byte])] = backingStore.get(eventIndex)

  def getRevealedValue(eventIndex: Int): Option[RevealedValue] =
    backingStore.get(eventIndex).map(stored => RevealedValue(writeToArray(stored.delta), stored.witness))

  def merged(using Lattice[Delta], Bottom[Delta]): Delta =
    backingStore.values.foldLeft(Bottom.empty)((acc, deltaValue) => acc.merge(deltaValue.delta))

  /** An independent copy of this store, unaffected by later puts into either one */
  def copy(): DeltaValueStore[Delta] = {
    val copied = DeltaValueStore[Delta]()
    copied.backingStore = backingStore
    copied
  }
}
