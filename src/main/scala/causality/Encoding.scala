package de.tu_darmstadt.stg.daimpl
package causality

import scala.math.pow

case class Encoding(bits: BigInt, digits: Int) {
  def addBits(n: Int, d: Int): Encoding = Encoding((bits << d) | n, digits + d)
  def popBits(d: Int): (BigInt, Encoding) = (bits & BigDecimal(pow(2, d+1)-1).toBigInt, Encoding((bits >> d), digits - d))

  def +(other: Encoding): Encoding = Encoding((bits << other.digits) | other.bits, digits + other.digits)

  override def toString(): String = String.format("%1$" + digits + "s", bits.toString(2)).replace(' ', '0');
  def toByteArray(): Array[Byte] = bits.toByteArray
  def length(): Int = digits
}

object Encoding {
  def apply(): Encoding = Encoding(0, 0)
}

object Encoder {
  def encode(idTree: IdTree, eventTree: EventTree): Encoding =
    encodeId(idTree) + encodeEvents(eventTree)

  def encodeId(idTree: IdTree): Encoding = {
    import causality.IdTree.{Branch, Leaf}
    idTree match
      case Leaf(0)                  => Encoding().addBits(0, 2).addBits(0, 1)
      case Leaf(1)                  => Encoding().addBits(0, 2).addBits(1, 1)
      case Branch(Leaf(0), i      ) => Encoding().addBits(1, 2) + encodeId(i)
      case Branch(i      , Leaf(0)) => Encoding().addBits(2, 2) + encodeId(i)
      case Branch(i1     , i2     ) => Encoding().addBits(3, 2) + encodeId(i1) + encodeId(i2)
  }

  def encodeEvents(eventTree: EventTree): Encoding = {
    import causality.EventTree.{Branch, Leaf}
    eventTree match
      case Branch(0, Leaf(0), e      ) => Encoding().addBits(0, 1).addBits(0, 2) + encodeEvents(e)
      case Branch(0, e      , Leaf(0)) => Encoding().addBits(0, 1).addBits(1, 2) + encodeEvents(e)
      case Branch(0, e1     , e2     ) => Encoding().addBits(0, 1).addBits(2, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Branch(n, Leaf(0), e      ) => Encoding().addBits(0, 1).addBits(3, 2).addBits(0, 1).addBits(0, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e      , Leaf(0)) => Encoding().addBits(0, 1).addBits(3, 2).addBits(0, 1).addBits(1, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e1     , e2     ) => Encoding().addBits(0, 1).addBits(3, 2).addBits(1, 1) + encodeNum(n, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Leaf(n)                     => Encoding().addBits(1, 1) + encodeNum(n, 2)
  }

  def encodeNum(n: Int, B: Int): Encoding = {
    if (n < pow(2, B)) {
      return Encoding().addBits(0, 1).addBits(n, B)
    } else {
      return Encoding().addBits(1, 1) + encodeNum(n - pow(2, B).intValue(), B+1)
    }
  }
}
