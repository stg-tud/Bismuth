package com.github.ckuessner
package codecs

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.ByteBuffer
import java.util.Base64

trait Encoder[T] {
  private val base64Encoder: Base64.Encoder = Base64.getEncoder
  private val base64Decoder: Base64.Decoder = Base64.getDecoder

  def write(obj: T, buffer: ByteBuffer): Unit

  def writeArray(obj: T): Array[Byte]

  def writeString(obj: T): String = {
    base64Encoder.encodeToString(writeArray(obj))
  }

  def read(buffer: ByteBuffer, length: Int): T

  def readArray(bytes: Array[Byte]): T

  def readString(bytes: String): T = {
    readArray(
      base64Decoder.decode(bytes)
    )
  }
}

trait FixedSizeEncoder[T] extends Encoder[T] {
  val BYTES: Int

  override def writeArray(obj: T): Array[Byte] = {
    val buffer = ByteBuffer.allocate(BYTES)
    write(obj, buffer)
    buffer.array()
  }

  override def readArray(bytes: Array[Byte]): T = {
    require(bytes.length == BYTES)
    val buffer = ByteBuffer.wrap(bytes)
    read(buffer, BYTES)
  }
}

trait VariableSizeEncoder[T] extends Encoder[T] {
  def BYTES(obj: T): Int

  override def writeArray(obj: T): Array[Byte] = {
    val buffer = ByteBuffer.allocate(BYTES(obj))
    write(obj, buffer)
    buffer.array()
  }

  override def readArray(bytes: Array[Byte]): T = {
    val buffer = ByteBuffer.wrap(bytes)
    read(buffer, bytes.length)
  }
}
