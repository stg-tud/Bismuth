package ex2026policyBroadcast

import channels.crypto.{PrivateIdentity, PublicIdentity, Signature, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.util

/** A delta, encoded as `payload`, signed by its `author`. The encoding is kept as signed, so verification does not
  * depend on the encoding being deterministic.
  */
final class Signed[S] private (val author: PublicIdentity, val payload: Array[Byte], val signature: Signature)(using
    JsonValueCodec[S]
) {
  lazy val delta: S = readFromArray[S](payload)

  def verify(using verifier: Verifier): Boolean = verifier.verify(author, payload, signature)

  override def equals(other: Any): Boolean = other match
      case other: Signed[?] => author == other.author && util.Arrays.equals(payload, other.payload)
      case _                => false
  override def hashCode(): Int  = author.hashCode * 31 + util.Arrays.hashCode(payload)
  override def toString: String = s"Signed(${author.id.take(8)}, $delta)"
}

object Signed {
  def apply[S: JsonValueCodec](author: PrivateIdentity, delta: S): Signed[S] =
      val payload = writeToArray(delta)
      new Signed(author.getPublic, payload, author.sign(payload))

  /** Encoded as (author, payload, signature). */
  given codec[S: JsonValueCodec]: JsonValueCodec[Signed[S]] = {
    given JsonValueCodec[Array[Byte]] = new JsonValueCodec[Array[Byte]] {
      def decodeValue(in: JsonReader, default: Array[Byte]): Array[Byte] = in.readBase64AsBytes(default)
      def encodeValue(x: Array[Byte], out: JsonWriter): Unit             = out.writeBase64Val(x, true)
      def nullValue: Array[Byte]                                         = Array.emptyByteArray
    }
    given JsonValueCodec[PublicIdentity]                                = JsonCodecMaker.make
    val tuple: JsonValueCodec[(PublicIdentity, Array[Byte], Signature)] = JsonCodecMaker.make
    new JsonValueCodec[Signed[S]] {
      def decodeValue(in: JsonReader, default: Signed[S]): Signed[S] =
          val (author, payload, signature) = tuple.decodeValue(in, tuple.nullValue)
          new Signed(author, payload, signature)
      def encodeValue(x: Signed[S], out: JsonWriter): Unit = tuple.encodeValue((x.author, x.payload, x.signature), out)
      def nullValue: Signed[S]                             = null.asInstanceOf[Signed[S]]
    }
  }
}
