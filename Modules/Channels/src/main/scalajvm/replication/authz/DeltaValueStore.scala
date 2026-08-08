package replication.authz

import crypto.Commitment.RevealedValue
import crypto.Hash

import java.util.concurrent.ConcurrentHashMap

class DeltaValueStore[Ardt] {
  private val backingStore: ConcurrentHashMap[Hash, RevealedValue] = new ConcurrentHashMap()

  def put(value: RevealedValue): Unit = backingStore.put(value.commitment, value): Unit

  def get(hash: Hash): Option[RevealedValue] = Option(backingStore.get(hash))
}
