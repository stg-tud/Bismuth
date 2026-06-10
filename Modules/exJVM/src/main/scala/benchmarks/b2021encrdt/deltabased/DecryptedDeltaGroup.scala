package benchmarks.b2021encrdt.deltabased

import channels.experiments.Aead
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.base.Lattice
import rdts.time.Dots

import java.nio.ByteBuffer

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: Dots) {
  def encrypt(aead: Aead)(using
      tJsonCodec: JsonValueCodec[T],
      dotSetJsonCodec: JsonValueCodec[Dots]
  ): EncryptedDeltaGroup = {
    val serialDeltaGroup          = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText      =
      aead.encrypt(ByteBuffer.wrap(serialDeltaGroup), ByteBuffer.wrap(serialDottedVersionVector))

    EncryptedDeltaGroup(deltaGroupCipherText, ByteBuffer.wrap(serialDottedVersionVector))
  }
}

object DecryptedDeltaGroup {
  given decryptedDeltaGroupSemiLattice[T](using
      tLattice: Lattice[T]
  ): Lattice[DecryptedDeltaGroup[T]] = (l, r) =>
    DecryptedDeltaGroup(
      Lattice.merge(l.deltaGroup, r.deltaGroup),
      l.dottedVersionVector.union(r.dottedVersionVector)
    )
}
