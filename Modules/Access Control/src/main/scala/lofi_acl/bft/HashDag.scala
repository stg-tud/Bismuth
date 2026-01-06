package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.*
import lofi_acl.bft.HashDag.{Encoder, Hash, Parents, hash}

import java.security.MessageDigest
import java.util.Base64

case class HashDag[Delta: {Encoder, Parents}](
    root: Hash,
    ops: Map[Hash, Delta],
    heads: Set[Hash],
) {
  // Returns either missing deltas, or the (potentially) updated op graph
  def add(delta: Delta): Either[Set[Hash], HashDag[Delta]] = {
    val deltaParents = summon[Parents[Delta]].parents(delta)
    val missing      = deltaParents.filterNot(ops.contains)
    if missing.nonEmpty then return Left(missing)
    val deltaHash = hash(delta)
    if ops.contains(deltaHash) then return Right(this)
    Right(copy(
      ops = ops + (deltaHash -> delta),
      heads = (heads -- deltaParents) + deltaHash,
    ))
  }
}

object HashDag {
  type Encoder[V] = V => Array[Byte]

  object Encoder {
    inline def fromJsoniter[RDT](using codec: JsonValueCodec[RDT]): Encoder[RDT] = rdt => writeToArray(rdt)(using codec)
  }

  trait Parents[Delta]:
      def parents(delta: Delta): Set[Hash]

  def hash[RDT: Encoder](rdt: RDT): Hash =
    new Hash(MessageDigest.getInstance("SHA3-256", "SUN").digest(summon[Encoder[RDT]].apply(rdt)))

  /** Container for SHA3-256 Hash. */
  class Hash private[HashDag] (private[HashDag] val delegate: Array[Byte]) {
    require(delegate.length == 32)
    def toBase64: String = Base64.getEncoder.encodeToString(delegate)

    override def toString: String = toBase64

    override def hashCode(): Int = delegate.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case otherHash: Hash => java.util.Arrays.equals(otherHash.delegate, delegate)
      case _               => false
    }
  }

  object Hash {
    def fromBase64(base64: String): Hash = Hash(Base64.getDecoder.decode(base64))
    def apply(hash: Array[Byte]): Hash   = new Hash(hash.clone())
  }

  given hashValueCodec: JsonValueCodec[Hash]:
      override def decodeValue(in: JsonReader, default: Hash): Hash = new Hash(in.readBase64AsBytes(Array.empty))
      override def encodeValue(x: Hash, out: JsonWriter): Unit      = out.writeBase64Val(x.delegate, true)
      override def nullValue: Hash                                  = null

  given hashKeyCodec: JsonKeyCodec[Hash]:
      override def decodeKey(in: JsonReader): Hash           = new Hash(in.readBase64AsBytes(Array.empty))
      override def encodeKey(x: Hash, out: JsonWriter): Unit = out.writeBase64Val(x.delegate, true)
}
