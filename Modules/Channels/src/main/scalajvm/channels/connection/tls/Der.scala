package channels.connection.tls

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.nio.charset.StandardCharsets

/** The small subset of ASN.1 DER needed to write and read the certificates of [[X509Util]]. */
private[tls] object Der {

  object Tag {
    val Integer: Int         = 0x02
    val BitString: Int       = 0x03
    val ObjectId: Int        = 0x06
    val Utf8String: Int      = 0x0c
    val UtcTime: Int         = 0x17
    val GeneralizedTime: Int = 0x18
    val Sequence: Int        = 0x30
    val Set: Int             = 0x31

    /** `[n] EXPLICIT`, constructed context-specific tag. */
    def explicit(n: Int): Int = 0xa0 | n
  }

  def encode(tag: Int, content: Array[Byte]): Array[Byte] = {
    val out = ByteArrayOutputStream()
    out.write(tag)
    val length = content.length
    if length < 0x80 then out.write(length)
    else
        val lengthBytes = BigInteger.valueOf(length).toByteArray.dropWhile(_ == 0)
        out.write(0x80 | lengthBytes.length)
        out.write(lengthBytes)
    out.write(content)
    out.toByteArray
  }

  def sequence(elements: Array[Byte]*): Array[Byte] = encode(Tag.Sequence, elements.toArray.flatten)
  def set(elements: Array[Byte]*): Array[Byte]      = encode(Tag.Set, elements.toArray.flatten)
  def explicit(n: Int, element: Array[Byte]): Array[Byte] = encode(Tag.explicit(n), element)

  def integer(value: BigInteger): Array[Byte] = encode(Tag.Integer, value.toByteArray)

  /** Object identifier, given as its DER content bytes. */
  def objectId(content: Array[Byte]): Array[Byte] = encode(Tag.ObjectId, content)

  def utf8String(value: String): Array[Byte] = encode(Tag.Utf8String, value.getBytes(StandardCharsets.UTF_8))

  /** Bit string without unused bits. */
  def bitString(bytes: Array[Byte]): Array[Byte] = encode(Tag.BitString, 0.toByte +: bytes)

  /** Strict reader for a single DER element, rejecting anything unexpected. */
  final class Reader(bytes: Array[Byte]) {
    private var position = 0

    def atEnd: Boolean = position == bytes.length

    /** Reads an element with tag `tag` and returns its content. */
    def read(tag: Int): Array[Byte] = {
      def next(): Int =
          if position >= bytes.length then throw IllegalArgumentException("unexpected end of DER data")
          val b = bytes(position) & 0xff
          position += 1
          b
      val actualTag = next()
      if actualTag != tag then throw IllegalArgumentException(s"expected DER tag $tag, got $actualTag")
      val first  = next()
      val length =
        if first < 0x80 then first
        else
            val count = first & 0x7f
            if count == 0 || count > 3 then throw IllegalArgumentException("unsupported DER length")
            val value = (0 until count).foldLeft(0)((acc, _) => (acc << 8) | next())
            if value < 0x80 then throw IllegalArgumentException("non-minimal DER length")
            value
      if length > bytes.length - position then throw IllegalArgumentException("unexpected end of DER data")
      val content = bytes.slice(position, position + length)
      position += length
      content
    }

    def readAll(tag: Int): Array[Byte] = {
      val content = read(tag)
      if !atEnd then throw IllegalArgumentException("unexpected trailing DER data")
      content
    }
  }
}
