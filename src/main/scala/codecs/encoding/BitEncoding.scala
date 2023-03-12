package de.tu_darmstadt.stg.daimpl
package codecs.encoding

import scala.annotation.targetName

case class BitEncoding(bits: BigInt, digits: Int) {
  def add(n: Int, d: Int): BitEncoding = BitEncoding(n << digits | bits, digits + d)
  def get(d: Int): Int = (bits & ((1 << d) - 1)).toInt
  def del(d: Int): BitEncoding = BitEncoding(bits >> d, digits-d)

  @targetName("sum")
  def +(other: BitEncoding): BitEncoding = BitEncoding((other.bits << digits) | bits, digits + other.digits)
  override def equals(other: Any): Boolean = other match {
    case other: BitEncoding => digits == other.digits && bits == other.bits
    case _ => false
  }

  override def toString: String = String.format("%1$" + digits + "s", bits.toString(2)).replace(' ', '0')

  def toByteArray: Array[Byte] = bits.toByteArray
  def length(): Int = digits
}

object BitEncoding {
  def apply(): BitEncoding = BitEncoding(0, 0)
  def fromString(str: String): BitEncoding = BitEncoding(BigInt(str, 2), str.length)
  def fromBytes(bytes: Array[Byte]): BitEncoding = BitEncoding(BigInt(bytes), bytes.length * 8)
}
