package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}

import java.security.MessageDigest
import java.util.Base64

/** Container for SHA3-256 Hash. */
class Hash private (private val delegate: Array[Byte]) {
  require(delegate.length == 32)

  def toBase64: String = Base64.getEncoder.encodeToString(delegate)

  def toArray: Array[Byte] = delegate.clone()

  override def toString: String = toBase64

  override def hashCode(): Int = delegate.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case otherHash: Hash => java.util.Arrays.equals(otherHash.delegate, delegate)
    case _               => false
  }
}

object Hash {
  def fromBase64(base64: String): Hash = Hash(Base64.getDecoder.decode(base64))

  def unsafeFromArray(hash: Array[Byte]): Hash = Hash(hash)

  def fromArray(hash: Array[Byte]): Hash = new Hash(hash.clone())

  def compute(data: Array[Byte]): Hash =
    new Hash(MessageDigest.getInstance("SHA3-256", "SUN").digest(data))

  given hashValueCodec: JsonValueCodec[Hash]:
      override def decodeValue(in: JsonReader, default: Hash): Hash = new Hash(in.readBase64AsBytes(Array.empty))

      override def encodeValue(x: Hash, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)

      override def nullValue: Hash = null

  given hashKeyCodec: JsonKeyCodec[Hash]:
      override def decodeKey(in: JsonReader): Hash = new Hash(in.readBase64AsBytes(Array.empty))

      override def encodeKey(x: Hash, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
}
