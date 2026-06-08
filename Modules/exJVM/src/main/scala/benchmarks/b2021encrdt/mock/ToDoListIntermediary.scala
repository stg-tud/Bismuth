package benchmarks.b2021encrdt.mock

import benchmarks.b2021encrdt.deltabased.{DeltaPruning, EncryptedDeltaGroup, UntrustedReplica}

class ToDoListIntermediary extends UntrustedReplica with DeltaPruning with IntermediarySizeInfo {
  def sizeInBytes: Long = {
    encryptedDeltaGroupStore.iterator.map { encDelta =>
      encDelta.serialDottedVersionVector.remaining().toLong + encDelta.stateCiphertext.remaining().toLong
    }.sum
  }

  def encDeltaCausalityInfoSizeInBytes: Long =
    encryptedDeltaGroupStore.iterator.map(_.serialDottedVersionVector.remaining().toLong).sum

  def rawDeltasSizeInBytes: Long =
    encryptedDeltaGroupStore.iterator.map(_.stateCiphertext.remaining().toLong).sum

  def numberStoredDeltas: Int = encryptedDeltaGroupStore.size

  override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {}
}
