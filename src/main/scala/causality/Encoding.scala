package de.tu_darmstadt.stg.daimpl
package causality

import scala.math.pow

case class Digit(num: Int, bits: Int) {
  def toBits(): String =
    String.format("%" + bits + "s", num.toBinaryString).replace(' ', '0')
}

case class Encoding(digits: Digit*) {
  def +(other: Encoding): Encoding =
    Encoding(digits ++ other.digits: _*)

  def toBits(): String =
    digits.map(_.toBits()).mkString("")
}

object Encoder {
  def encode(idTree: IdTree, eventTree: EventTree): Encoding =
    encodeId(idTree) + encodeEvents(eventTree)

  private def encodeId(idTree: IdTree): Encoding = {
    import causality.IdTree.{Branch, Leaf}
    idTree match 
      case Leaf(0)                  => Encoding(Digit(0, 2), Digit(0, 1))
      case Leaf(1)                  => Encoding(Digit(0, 2), Digit(1, 1))
      case Branch(Leaf(0), i      ) => Encoding(Digit(1, 2)) + encodeId(i)
      case Branch(i      , Leaf(0)) => Encoding(Digit(2, 2)) + encodeId(i)
      case Branch(i1     , i2     ) => Encoding(Digit(3, 2)) + encodeId(i1) + encodeId(i2)
  }

  private def encodeEvents(eventTree: EventTree): Encoding =
    import causality.EventTree.{Branch, Leaf}
    eventTree match 
      case Branch(0, Leaf(0), e      ) => Encoding(Digit(0, 1), Digit(0, 2)) + encodeEvents(e)
      case Branch(0, e      , Leaf(0)) => Encoding(Digit(0, 1), Digit(1, 2)) + encodeEvents(e)
      case Branch(0, e1     , e2     ) => Encoding(Digit(0, 1), Digit(2, 2)) + encodeEvents(e1) + encodeEvents(e2)
      case Branch(n, Leaf(0), e      ) => Encoding(Digit(0, 1), Digit(3, 2), Digit(0, 1), Digit(0, 1)) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e      , Leaf(0)) => Encoding(Digit(0, 1), Digit(3, 2), Digit(0, 1), Digit(1, 1)) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e1     , e2     ) => Encoding(Digit(0, 1), Digit(3, 2), Digit(1, 1)) + encodeNum(n, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Leaf(n)                     => Encoding(Digit(1, 1)) + encodeNum(n, 2)

  private def encodeNum(n: Int, B: Int): Encoding = {
    if (n < pow(2, B)) {
      return Encoding(Digit(0, 1), Digit(n, B))
    } else {
      return Encoding(Digit(1, 1)) + encodeNum(n - pow(2, B).intValue(), B+1)
    }
  }
}
