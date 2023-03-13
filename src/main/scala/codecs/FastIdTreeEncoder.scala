package de.tu_darmstadt.stg.daimpl
package codecs

import causality.{IdTree, IntervalTreeClock}
import codecs.internal.{IdTreeDecoder, IdTreeEncoder}

import java.nio.ByteBuffer

given FastIdTreeEncoder: Encoder[IdTree] with {
  override def write(idTree: IdTree, buffer: ByteBuffer): Unit = {
    buffer.put(writeArray(idTree))
  }

  override def writeArray(idTree: IdTree): Array[Byte] = {
    IdTreeEncoder.encode(idTree).toByteArray
  }

  override def read(buffer: ByteBuffer, length: Int): IdTree = {
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): IdTree = {
    IdTreeDecoder.decode(bytes)
  }
}
