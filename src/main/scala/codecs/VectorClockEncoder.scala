package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Defs.{Id, Time}
import causality.dots.VectorClock

import java.nio.ByteBuffer

object VectorClockEncoder extends VariableSizeEncoder[VectorClock] {
  override protected inline def BYTES(obj: VectorClock): Int =
    obj.timestamps.size * java.lang.Long.BYTES * 2

  override def write(obj: VectorClock, buffer: ByteBuffer): Unit = {
    obj.timestamps.foreach { (id, time) =>
      buffer.putLong(id)
      buffer.putLong(time)
    }
  }

  override def readArray(bytes: Array[Byte]): VectorClock = {
    require(bytes.length % 2 * java.lang.Long.BYTES == 0)

    val mapBuilder = Map.newBuilder[Id, Time]
    mapBuilder.sizeHint(1)
    val buffer = ByteBuffer.wrap(bytes)

    Range(0, bytes.length, 2 * java.lang.Long.BYTES).foreach(_ => {
      val id   = buffer.getLong
      val time = buffer.getLong
      require(time >= 0)
      mapBuilder += id -> time
    })

    VectorClock(mapBuilder.result())
  }
}
