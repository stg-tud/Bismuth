package channels.crypto

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import pt.kcry.blake3.Blake3

import java.util
import java.util.Base64

/** Container for a 256 bit BLAKE3 hash. */
class Hash private (private val delegate: Array[Byte]) {
  require(delegate.length == Hash.length)

  def toBase64: String = Base64.getEncoder.encodeToString(delegate)

  def toArray: Array[Byte] = delegate.clone()

  override def toString: String = toBase64

  override def hashCode(): Int = util.Arrays.hashCode(delegate)

  override def equals(obj: Any): Boolean = obj match {
    case otherHash: Hash => java.util.Arrays.equals(otherHash.delegate, delegate)
    case _               => false
  }
}

object Hash {
  val length = 32

  val allZeroHash: Hash = Hash.unsafeFromArray(Array.ofDim(length))

  def fromBase64(base64: String): Hash = Hash(Base64.getDecoder.decode(base64))

  def unsafeFromArray(hash: Array[Byte]): Hash = Hash(hash)

  def fromArray(hash: Array[Byte]): Hash = new Hash(hash.clone())

  def compute(data: Array[Byte]): Hash = new Hash(Blake3.hash(data, length))

  /** Hash of the concatenation of `parts`. */
  def computeConcatenated(parts: Array[Byte]*): Hash = {
    val hasher = Blake3.newHasher()
    parts.foreach(part => hasher.update(part): Unit)
    new Hash(hasher.done(length))
  }

  given hashValueCodec: JsonValueCodec[Hash]:
      override def decodeValue(in: JsonReader, default: Hash): Hash = new Hash(in.readBase64AsBytes(Array.empty))

      override def encodeValue(x: Hash, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)

      override def nullValue: Hash = null.asInstanceOf[Hash]

  given hashKeyCodec: JsonKeyCodec[Hash]:
      override def decodeKey(in: JsonReader): Hash = new Hash(in.readBase64AsBytes(Array.empty))

      override def encodeKey(x: Hash, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
}
