package com.github.ckuessner
package codecs

import causality.dots.Defs.Id
import causality.dots.DotSet
import causality.dots.impl.ArrayRanges

import java.nio.ByteBuffer

given DotSetEncoder: VariableSizeEncoder[DotSet] with {
  override def BYTES(obj: DotSet): Int = {
    val bytesForArrayRanges = obj.internal.map { case (_, arrayRanges) =>
      java.lang.Integer.BYTES + arrayRanges.used * java.lang.Long.BYTES
    }.sum
    val bytesForDottedVersionVectorLength = java.lang.Integer.BYTES
    val bytesForIds                       = obj.internal.size * java.lang.Long.BYTES

    bytesForArrayRanges + bytesForDottedVersionVectorLength + bytesForIds
  }

  override def write(obj: DotSet, buffer: ByteBuffer): Unit = {
    // Number of entries in dotted version vector
    buffer.putInt(obj.internal.size)
    obj.internal.foreach { (id, arrayRanges) =>
      // Id for ranges
      buffer.putLong(id)
      // Number of entries in ArrayRanges
      buffer.putInt(arrayRanges.used)
      // ArrayRanges
      ArrayRangesEncoder.write(arrayRanges, buffer)
    }
  }

  override def read(buffer: ByteBuffer, length: Int): DotSet = {
    var remainingBytes = length
    require(remainingBytes >= java.lang.Integer.BYTES)
    val numEntries: Int = buffer.getInt()
    remainingBytes -= java.lang.Integer.BYTES
    require(numEntries >= 0)

    val mapBuilder = Map.newBuilder[Id, ArrayRanges]
    mapBuilder.sizeHint(numEntries)

    (0 until numEntries).foreach { _ =>
      require(remainingBytes >= 2 * java.lang.Long.BYTES)
      val id: Id              = buffer.getLong()
      val bytesForArrayRanges = buffer.getInt() * java.lang.Long.BYTES
      val arrayRanges         = ArrayRangesEncoder.read(buffer, bytesForArrayRanges)
      mapBuilder += id -> arrayRanges
    }

    DotSet(mapBuilder.result())
  }
}
