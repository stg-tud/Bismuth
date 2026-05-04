package rdts.time.codecs

import java.nio.ByteBuffer
import java.util.Base64

trait Codec[T] {
  private val base64Encoder: Base64.Encoder = Base64.getEncoder
  private val base64Decoder: Base64.Decoder = Base64.getDecoder

  def write(obj: T, buffer: ByteBuffer): Unit

  def writeArray(obj: T): Array[Byte]

  def writeString(obj: T): String =
    base64Encoder.encodeToString(writeArray(obj))

  def read(buffer: ByteBuffer, length: Int): T

  def readArray(bytes: Array[Byte]): T

  def readString(bytes: String): T = {
    readArray(
      base64Decoder.decode(bytes)
    )
  }
}
