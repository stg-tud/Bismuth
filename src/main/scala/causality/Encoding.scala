package de.tu_darmstadt.stg.daimpl
package causality

import scala.math.pow

case class Encoding(bits: BigInt, digits: Int) {
  def add(n: Int, d: Int): Encoding = Encoding((bits << d) | n, digits + d)
  def get(d: Int): Int = ((bits & ((1 << d) - 1 << digits-d)) >> digits-d).toInt
  def del(d: Int): Encoding = Encoding(bits & (1 << digits-d) - 1, digits-d)

  def +(other: Encoding): Encoding = Encoding((bits << other.digits) | other.bits, digits + other.digits)
  override def equals(other: Any): Boolean = other match {
    case other: Encoding => digits == other.digits &&
      (bits & (1 << digits) - 1) == (other.bits & (1 << other.digits) - 1)
    case _ => false
  }

  override def toString(): String = String.format("%1$" + digits + "s", bits.toString(2)).replace(' ', '0');
  def toByteArray(): Array[Byte] = bits.toByteArray
  def length(): Int = digits
}

object Encoding {
  def apply(): Encoding = Encoding(0, 0)
  def fromString(str: String): Encoding = {
    str.split("").map(c => Encoding(if (c == "1") 1 else 0, 1)).foldLeft(Encoding())(_ + _)
  }
}

object Encoder {
  def encode(idTree: IdTree, eventTree: EventTree): Encoding =
    encodeId(idTree) + encodeEvents(eventTree)

  def encodeId(idTree: IdTree): Encoding = {
    import causality.IdTree.{Branch, Leaf}
    idTree match
      case Leaf(0)                  => Encoding().add(0, 2).add(0, 1)
      case Leaf(1)                  => Encoding().add(0, 2).add(1, 1)
      case Branch(Leaf(0), i      ) => Encoding().add(1, 2) + encodeId(i)
      case Branch(i      , Leaf(0)) => Encoding().add(2, 2) + encodeId(i)
      case Branch(i1     , i2     ) => Encoding().add(3, 2) + encodeId(i1) + encodeId(i2)
  }

  def encodeEvents(eventTree: EventTree): Encoding = {
    import causality.EventTree.{Branch, Leaf}
    eventTree match
      case Branch(0, Leaf(0), e      ) => Encoding().add(0, 1).add(0, 2) + encodeEvents(e)
      case Branch(0, e      , Leaf(0)) => Encoding().add(0, 1).add(1, 2) + encodeEvents(e)
      case Branch(0, e1     , e2     ) => Encoding().add(0, 1).add(2, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Branch(n, Leaf(0), e      ) => Encoding().add(0, 1).add(3, 2).add(0, 1).add(0, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e      , Leaf(0)) => Encoding().add(0, 1).add(3, 2).add(0, 1).add(1, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e1     , e2     ) => Encoding().add(0, 1).add(3, 2).add(1, 1) + encodeNum(n, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Leaf(n)                     => Encoding().add(1, 1) + encodeNum(n, 2)
  }

  def encodeNum(n: Int, B: Int): Encoding = {
    if (n < pow(2, B)) {
      return Encoding().add(0, 1).add(n, B)
    } else {
      return Encoding().add(1, 1) + encodeNum(n - (1 << B), B+1)
    }
  }
}
