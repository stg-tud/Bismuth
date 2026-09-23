package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import crypto.Commitment.RevealedValue
import crypto.Hash
import rdts.base.{Bottom, Lattice}

import scala.collection.mutable

// TODO: Alternatively, we could store the Delta along with the salt. This might be faster for sending and rematerialization
class DeltaValueStore[Delta] {
  private val backingStore: mutable.Map[Hash, RevealedValue] = mutable.Map.empty

  def put(value: RevealedValue): Unit = backingStore.put(value.commitment, value): Unit

  /** Assumes that commitment is correct */
  def put(commitment: Hash, value: RevealedValue): Unit = backingStore.put(commitment, value): Unit

  def remove(commitment: Hash): Option[RevealedValue] = backingStore.remove(commitment)

  def get(hash: Hash): Option[RevealedValue] = backingStore.get(hash)

  def merged(using Lattice[Delta], Bottom[Delta], JsonValueCodec[Delta]): Delta =
    backingStore.values.foldLeft(Bottom.empty)((l, r) =>
        val delta = readFromArray(r.value)
        l.merge(delta)
    )

  /** An independent copy of this store, unaffected by later puts into either one */
  def copy(): DeltaValueStore[Delta] = {
    val copied = DeltaValueStore[Delta]()
    copied.backingStore ++= backingStore
    copied
  }
}
