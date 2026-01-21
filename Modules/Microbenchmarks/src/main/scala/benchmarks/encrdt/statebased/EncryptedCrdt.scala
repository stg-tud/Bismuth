package benchmarks.encrdt.statebased

import benchmarks.encrdt.statebased.DecryptedState.vectorClockJsonCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Lattice
import rdts.time.VectorClock
import replication.Aead
import replication.JsoniterCodecs.given

case class EncryptedState(stateCiphertext: Array[Byte], serialVersionVector: Array[Byte]) {
  lazy val versionVector: VectorClock = readFromArray[VectorClock](serialVersionVector)

  def decrypt[T](aead: Aead)(using tJsonCodec: JsonValueCodec[T]): DecryptedState[T] = {
    val plainText     = aead.decrypt(stateCiphertext, serialVersionVector).get
    val state         = readFromArray[T](plainText)
    val versionVector = readFromArray[VectorClock](serialVersionVector)
    DecryptedState(state, versionVector)
  }
}

object EncryptedState {
  given encStateJsonCodec: JsonValueCodec[EncryptedState] = JsonCodecMaker.make
}

case class DecryptedState[T](state: T, versionVector: VectorClock) {
  def encrypt(aead: Aead)(using tJsonCodec: JsonValueCodec[T]): EncryptedState = {
    val serialVectorClock = writeToArray(versionVector)
    val stateCipherText   = aead.encrypt(
      writeToArray(state),
      serialVectorClock
    )
    EncryptedState(stateCipherText, serialVectorClock)
  }
}

object DecryptedState {
  given vectorClockJsonCodec: JsonValueCodec[VectorClock] = JsonCodecMaker.make

  given lattice[T](using tLattice: Lattice[T]): Lattice[DecryptedState[T]] = (left, right) =>
    DecryptedState(Lattice.merge(left.state, right.state), left.versionVector.merge(right.versionVector))
}
