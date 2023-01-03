package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Defs.{Id, Time}
import causality.dots.Dot

import java.nio.ByteBuffer
import scala.compiletime.ops.string.Length

object DotEncoder extends FixedSizeEncoder[Dot] {
  val BYTES: Int = 2 * java.lang.Long.BYTES

  override inline def write(dot: Dot, buffer: ByteBuffer): Unit = {
    buffer.putLong(dot.time)
    buffer.putLong(dot.replicaId)
    buffer.array()
  }

  override inline def read(buffer: ByteBuffer, length: Int): Dot = {
    require(length == 2 * java.lang.Long.BYTES)
    val time: Time = buffer.getLong(0)
    require(time >= 0)
    val id: Id = buffer.getLong(8)
    Dot(time, id)
  }
}
