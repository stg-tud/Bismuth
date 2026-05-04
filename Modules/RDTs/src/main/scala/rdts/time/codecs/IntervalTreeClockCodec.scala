package rdts.time.codecs

import rdts.time.IntervalTreeClock

import java.nio.ByteBuffer

given IntervalTreeClockCodec: Codec[IntervalTreeClock] with {
  override def write(itc: IntervalTreeClock, buffer: ByteBuffer): Unit =
    buffer.put(writeArray(itc)): Unit

  override def writeArray(itc: IntervalTreeClock): Array[Byte] =
    internal.IntervalTreeClockEncoder.encode(itc)

  override def read(buffer: ByteBuffer, length: Int): IntervalTreeClock = {
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): IntervalTreeClock =
    internal.IntervalTreeClockDecoder.decode(bytes)
}
