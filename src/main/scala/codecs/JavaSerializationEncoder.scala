package de.tu_darmstadt.stg.daimpl
package codecs

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.ByteBuffer

given JavaSerializationEncoder[T]: Encoder[T] with {
  override def write(obj: T, buffer: ByteBuffer): Unit = {
    buffer.put(writeArray(obj))
  }

  override def writeArray(obj: T): Array[Byte] = {
    val bs: ByteArrayOutputStream = ByteArrayOutputStream()
    val os                        = ObjectOutputStream(bs)
    os.writeObject(obj)
    os.close()
    bs.toByteArray
  }

  override def read(buffer: ByteBuffer, length: Int): T = {
    val bytes: Array[Byte] = new Array[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): T = {
    val os = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val v  = os.readObject
    os.close()
    v.asInstanceOf[T]
  }
}
