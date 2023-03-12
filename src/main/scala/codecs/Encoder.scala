package de.tu_darmstadt.stg.daimpl
package codecs

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
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

  def writeObject(obj: T, buffer: ByteBuffer): Unit = writeObjectArray(obj).toBuffer

  def writeObjectArray(obj: T): Array[Byte] = {
    val bs: ByteArrayOutputStream = ByteArrayOutputStream()
    val os = ObjectOutputStream(bs)
    os.writeObject(obj)
    os.close()
    bs.toByteArray
  }

  def writeObjectString(obj: T): String = {
    base64Encoder.encodeToString(writeObjectArray(obj))
  }

  def read(buffer: ByteBuffer, length: Int): T

  def readArray(bytes: Array[Byte]): T

  def readString(bytes: String): T = {
    readArray(
      base64Decoder.decode(bytes)
    )
  }

  def readObject(buffer: ByteBuffer, length: Int): T = readObjectArray(buffer.array())

  def readObjectArray(bytes: Array[Byte]): T = {
    val os = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val v = os.readObject
    os.close()
    v.asInstanceOf[T]
  }

  def readObjectString(bytes: String): T = {
    readObjectArray(
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
