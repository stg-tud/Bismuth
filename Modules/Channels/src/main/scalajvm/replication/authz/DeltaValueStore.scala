package replication.authz

import crypto.Commitment.RevealedValue
import crypto.Hash

import scala.collection.mutable

class DeltaValueStore[Delta] {
  private val backingStore: mutable.Map[Hash, RevealedValue] = mutable.Map.empty

  def put(value: RevealedValue): Unit = backingStore.put(value.commitment, value): Unit

  def get(hash: Hash): Option[RevealedValue] = backingStore.get(hash)

  /** An independent copy of this store, unaffected by later puts into either one */
  def copy(): DeltaValueStore[Delta] = {
    val copied = DeltaValueStore[Delta]()
    copied.backingStore ++= backingStore
    copied
  }
}
