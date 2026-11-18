package channels.crypto

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}

import java.util
import java.util.Base64

/** Container for Ed25519 signature. */
class Signature private (private val delegate: Array[Byte]) {
  def toBase64: String = Base64.getEncoder.encodeToString(delegate)

  def toArray: Array[Byte] = delegate.clone()

  override def toString: String = toBase64

  override def hashCode(): Int = util.Arrays.hashCode(delegate)

  override def equals(obj: Any): Boolean = obj match {
    case otherSignature: Signature => java.util.Arrays.equals(otherSignature.delegate, delegate)
    case _                         => false
  }
}

object Signature {
  def apply(signatureBytes: Array[Byte]): Signature = new Signature(signatureBytes.clone())

  val length: Int = 64

  val allZeroSignature: Signature = Signature.unsafeFromArray(Array.ofDim(length))

  def unsafeFromArray(signatureBytes: Array[Byte]): Signature = new Signature(signatureBytes)

  given signatureValueCodec: JsonValueCodec[Signature]:
      override def decodeValue(in: JsonReader, default: Signature): Signature =
        new Signature(in.readBase64AsBytes(Array.empty))
      override def encodeValue(x: Signature, out: JsonWriter): Unit =
        if x eq null then out.writeNull()
        else out.writeBase64Val(x.delegate, true)
      override def nullValue: Signature = null.asInstanceOf[Signature]

  given signatureKeyCodec: JsonKeyCodec[Signature]:
      override def decodeKey(in: JsonReader): Signature           = new Signature(in.readBase64AsBytes(Array.empty))
      override def encodeKey(x: Signature, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
}
