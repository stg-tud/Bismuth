package com.github.ckuessner
package codecs

import causality.IntervalTreeClock

import java.nio.ByteBuffer

given IntervalTreeClockEncoder: Encoder[IntervalTreeClock] with {
  override def write(itc: IntervalTreeClock, buffer: ByteBuffer): Unit = {
    buffer.put(writeArray(itc))
  }

  override def writeArray(itc: IntervalTreeClock): Array[Byte] = {
    internal.IntervalTreeClockEncoder.encode(itc)
  }

  override def read(buffer: ByteBuffer, length: Int): IntervalTreeClock = {
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): IntervalTreeClock = {
    internal.IntervalTreeClockDecoder.decode(bytes)
  }
}
