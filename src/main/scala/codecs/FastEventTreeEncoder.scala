package de.tu_darmstadt.stg.daimpl
package codecs

import causality.EventTree
import codecs.internal.{EventTreeDecoder, EventTreeEncoder}

import java.nio.ByteBuffer

given FastEventTreeEncoder: Encoder[EventTree] with {
  override def write(eventTree: EventTree, buffer: ByteBuffer): Unit = {
    buffer.put(writeArray(eventTree))
  }

  override def writeArray(eventTree: EventTree): Array[Byte] = {
    EventTreeEncoder.encode(eventTree).toByteArray
  }

  override def read(buffer: ByteBuffer, length: Int): EventTree = {
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): EventTree = {
    EventTreeDecoder.decode(bytes)
  }
}
