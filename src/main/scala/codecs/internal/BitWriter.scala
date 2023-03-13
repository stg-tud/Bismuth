package de.tu_darmstadt.stg.daimpl
package codecs.internal

import scala.annotation.tailrec
import scala.collection.mutable

private[internal] class BitWriter {
  private val byteArray: mutable.ArrayBuffer[Byte] = mutable.ArrayBuffer.empty
  private var pos: Int                             = 0

  def write(number: Int, bits: Int): Unit = {
    assert(number >= 0)
    assert(number < (1.longValue << bits))

    if (pos % 8 == 0) {
      writeByteAligned(number, bits)
    } else {
      val fillableBitsInCurrentByte = 8 - (pos % 8)

      if (fillableBitsInCurrentByte >= bits) {
        val partOfNumberToAdd: Byte = (number << (fillableBitsInCurrentByte - bits)).toByte
        byteArray(pos / 8) = ((byteArray(pos / 8) & 0xff) | (partOfNumberToAdd & 0xff)).toByte

        pos += bits
      } else {
        val partOfNumberToAdd: Byte = (number >>> (bits - fillableBitsInCurrentByte)).toByte
        byteArray(pos / 8) = ((byteArray(pos / 8) & 0xff) | (partOfNumberToAdd & 0xff)).toByte

        pos += fillableBitsInCurrentByte

        val lowerPartOfNumber = number & ((1 << (bits - fillableBitsInCurrentByte)) - 1)
        writeByteAligned(lowerPartOfNumber, bits - fillableBitsInCurrentByte)
      }
    }
  }

  @tailrec
  private def writeByteAligned(number: Int, bits: Int): Unit = {
    assert(pos % 8 == 0)
    assert(number >= 0)
    assert(number < (1.longValue << bits))

    if (bits <= 8) {
      val byte: Byte = (number << (8 - bits)).toByte
      byteArray.append(byte)
      pos += bits
    } else {
      val firstByte: Byte = (number >>> (bits - 8)).toByte
      byteArray.append(firstByte)
      pos += 8

      val lowerPartOfNumber = number & ((1 << (bits - 8)) - 1)
      writeByteAligned(lowerPartOfNumber, bits - 8)
    }
  }

  def toByteArray: Array[Byte] = byteArray.toArray
}

private[internal] object BitWriter {
  def empty: BitWriter = new BitWriter()
}
