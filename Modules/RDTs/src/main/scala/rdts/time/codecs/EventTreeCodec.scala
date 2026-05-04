package rdts.time.codecs

import rdts.time.EventTree
import rdts.time.codecs.internal.{EventTreeDecoder, EventTreeEncoder}

import java.nio.ByteBuffer

given EventTreeCodec: Codec[EventTree] with {
  override def write(eventTree: EventTree, buffer: ByteBuffer): Unit =
    buffer.put(writeArray(eventTree)): Unit

  override def writeArray(eventTree: EventTree): Array[Byte] =
    EventTreeEncoder.encode(eventTree).toByteArray

  override def read(buffer: ByteBuffer, length: Int): EventTree = {
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): EventTree =
    EventTreeDecoder.decode(bytes)
}
