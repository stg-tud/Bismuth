package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Defs.Time
import causality.dots.impl.ArrayRanges

import java.nio.ByteBuffer

given ArrayRangesEncoder: VariableSizeEncoder[ArrayRanges] with {
  override protected def BYTES(obj: ArrayRanges): Int =
    obj.used * java.lang.Long.BYTES

  override def write(obj: ArrayRanges, buffer: ByteBuffer): Unit =
    val longs: Array[Time] = obj.inner
    for (i <- 0 until obj.used) {
      buffer.putLong(longs(i))
    }

  override def read(buffer: ByteBuffer, length: Int): ArrayRanges = {
    require(length % java.lang.Long.BYTES * 2 == 0)
    val times = Array.ofDim[Time](length / java.lang.Long.BYTES)
    times.indices.foreach { i =>
      times(i) = buffer.getLong()
    }

    ArrayRanges(times, times.length)
  }
}
