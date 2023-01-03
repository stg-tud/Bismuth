package de.tu_darmstadt.stg.daimpl
package codecs

import java.nio.ByteBuffer
import java.util.Base64

trait Encoder[T] {
  private val base64Encoder: Base64.Encoder = Base64.getEncoder
  private val base64Decoder: Base64.Decoder = Base64.getDecoder

  def writeArray(obj: T): Array[Byte]

  def write(obj: T, buffer: ByteBuffer): Unit

  def writeString(obj: T): String = {
    base64Encoder.encodeToString(writeArray(obj))
  }

  def read(buffer: ByteBuffer): T

  def readArray(bytes: Array[Byte]): T

  def readString(bytes: String): T = {
    readArray(
      base64Decoder.decode(bytes)
    )
  }
}

trait FixedSizeEncoder[T] extends Encoder[T] {
  val BYTES: Int

  override def readArray(bytes: Array[Byte]): T = {
    require(bytes.length == BYTES)
    read(ByteBuffer.wrap(bytes))
  }

  override def writeArray(obj: T): Array[Byte] = {
    val buffer = ByteBuffer.allocate(BYTES)
    write(obj, buffer)
    buffer.array()
  }
}
