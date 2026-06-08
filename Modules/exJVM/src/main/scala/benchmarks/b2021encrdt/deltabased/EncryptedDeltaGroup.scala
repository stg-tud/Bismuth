package benchmarks.b2021encrdt.deltabased

import channels.experiments.Aead
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer}
import rdts.time.Dots

import java.nio.ByteBuffer

case class EncryptedDeltaGroup(stateCiphertext: ByteBuffer, serialDottedVersionVector: ByteBuffer)(
    implicit dotSetJsonCodec: JsonValueCodec[Dots]
) {
  lazy val dottedVersionVector: Dots = readFromByteBuffer(serialDottedVersionVector.duplicate())

  def decrypt[T](aead: Aead)(using tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText           = aead.decrypt(stateCiphertext.duplicate(), serialDottedVersionVector.duplicate()).get
    val state               = readFromByteBuffer[T](plainText)
    val dottedVersionVector = readFromByteBuffer[Dots](serialDottedVersionVector.duplicate())
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}
