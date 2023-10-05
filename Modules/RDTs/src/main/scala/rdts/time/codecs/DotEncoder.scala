package com.github.ckuessner
package codecs

import causality.dots.Defs.{Id, Time}
import causality.dots.Dot

import java.nio.ByteBuffer
import scala.compiletime.ops.string.Length

given DotEncoder: FixedSizeEncoder[Dot] with {
  val BYTES: Int = 2 * java.lang.Long.BYTES

  override inline def write(dot: Dot, buffer: ByteBuffer): Unit = {
    buffer.putLong(dot.replicaId)
    buffer.putLong(dot.time)
    buffer.array()
  }

  override inline def read(buffer: ByteBuffer, length: Int): Dot = {
    require(length == 2 * java.lang.Long.BYTES)
    val replicaId: Id = buffer.getLong()
    val time: Time = buffer.getLong()
    require(time >= 0)
    Dot(replicaId, time)
  }
}
