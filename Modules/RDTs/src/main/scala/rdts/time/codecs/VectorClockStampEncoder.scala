package com.github.ckuessner
package codecs

import causality.dots.Defs.Id
import causality.dots.VectorClock
import codecs.{Encoder, VariableSizeEncoder, VectorClockEncoder}

import java.nio.ByteBuffer

given VectorClockStampEncoder: VariableSizeEncoder[(Id, VectorClock)] with {
  override def BYTES(obj: (Id, VectorClock)): Int = java.lang.Long.BYTES + VectorClockEncoder.BYTES(obj._2)

  override def write(obj: (Id, VectorClock), buffer: ByteBuffer): Unit = {
    buffer.putLong(obj._1)
    VectorClockEncoder.write(obj._2, buffer)
  }

  override def read(buffer: ByteBuffer, length: Int): (Id, VectorClock) = {
    require(length >= java.lang.Long.BYTES)
    val id              = buffer.getLong()
    val remainingLength = length - java.lang.Long.BYTES
    val vc =
      if (remainingLength > 0) VectorClockEncoder.read(buffer, remainingLength)
      else VectorClock()
    (id, vc)
  }
}
