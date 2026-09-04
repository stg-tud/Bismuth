package replication.authz

import crypto.Commitment.RevealedValue
import crypto.Hash

import scala.collection.mutable

class DeltaValueStore[Delta] {
  private val backingStore: mutable.Map[Hash, RevealedValue] = mutable.Map.empty

  def put(value: RevealedValue): Unit = backingStore.put(value.commitment, value): Unit

  def get(hash: Hash): Option[RevealedValue] = backingStore.get(hash)
}
