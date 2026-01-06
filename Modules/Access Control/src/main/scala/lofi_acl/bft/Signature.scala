package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import crypto.Ed25519Util

import java.security.PublicKey
import java.util.Base64

/** Container for Ed25519 signature. */
class Signature private (private val delegate: Array[Byte]) {
  require(delegate.length == 64)

  def toBase64: String = Base64.getEncoder.encodeToString(delegate)

  override def toString: String = toBase64

  override def hashCode(): Int = delegate.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case otherSignature: Signature => java.util.Arrays.equals(otherSignature.delegate, delegate)
    case _                         => false
  }

  def verify(signingKey: PublicKey, data: Array[Byte]): Boolean =
    Ed25519Util.checkEd25519Signature(data, delegate, signingKey)
}

object Signature {
  def apply(signatureBytes: Array[Byte]): Signature = new Signature(signatureBytes.clone())

  def unsafeFromArray(signatureBytes: Array[Byte]): Signature = new Signature(signatureBytes)
  given signatureValueCodec: JsonValueCodec[Signature]:
      override def decodeValue(in: JsonReader, default: Signature): Signature =
        new Signature(in.readBase64AsBytes(Array.empty))
      override def encodeValue(x: Signature, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
      override def nullValue: Signature                             = null

  given signatureKeyCodec: JsonKeyCodec[Signature]:
      override def decodeKey(in: JsonReader): Signature           = new Signature(in.readBase64AsBytes(Array.empty))
      override def encodeKey(x: Signature, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
}
