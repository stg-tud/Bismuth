package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.Hash
import rdts.base.{Bottom, Lattice}

import scala.collection.mutable

class DeltaValueStore[Delta: JsonValueCodec] {
  // Stores delta and witness (salt of commitment)
  private val backingStore: mutable.Map[Hash, (delta: Delta, witness: Array[Byte])] = mutable.Map.empty

  def put(value: RevealedValue): Unit =
    put(value.commitment, readFromArray(value.value), value.witness)

  /** Assumes that commitment is correct */
  def put(commitment: Hash, value: Delta, witness: Array[Byte]): Unit =
    backingStore.put(commitment, (value, witness)): Unit

  def remove(commitment: Hash): Option[(delta: Delta, witness: Array[Byte])] = backingStore.remove(commitment)

  def get(commitment: Hash): Option[(delta: Delta, witness: Array[Byte])] = backingStore.get(commitment)

  def getRevealedValue(commitment: Hash): Option[RevealedValue] =
    backingStore.get(commitment).map(stored => RevealedValue(writeToArray(stored.delta), stored.witness))

  def merged(using Lattice[Delta], Bottom[Delta]): Delta =
    backingStore.values.foldLeft(Bottom.empty)((acc, deltaValue) => acc.merge(deltaValue.delta))

  /** An independent copy of this store, unaffected by later puts into either one */
  def copy(): DeltaValueStore[Delta] = {
    val copied = DeltaValueStore[Delta]()
    copied.backingStore ++= backingStore
    copied
  }
}
