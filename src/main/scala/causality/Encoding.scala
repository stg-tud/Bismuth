package de.tu_darmstadt.stg.daimpl
package causality

import scala.math.pow

case class Encoding(bits: BigInt, digits: Int) {
  def add(n: Int, d: Int): Encoding = Encoding(n << digits | bits, digits + d)
  def get(d: Int): Int = (bits & ((1 << d) - 1)).toInt
  def del(d: Int): Encoding = Encoding(bits >> d, digits-d)

  def +(other: Encoding): Encoding = Encoding((other.bits << digits) | bits, digits + other.digits)
  override def equals(other: Any): Boolean = other match {
    case other: Encoding => digits == other.digits && bits == other.bits
    case _ => false
  }

  override def toString(): String = String.format("%1$" + digits + "s", bits.toString(2)).replace(' ', '0');
  def toByteArray(): Array[Byte] = bits.toByteArray
  def length(): Int = digits
}

object Encoding {
  def apply(): Encoding = Encoding(0, 0)
  def fromString(str: String): Encoding = Encoding(BigInt(str, 2), str.length)
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

object Decoder {
  def decode(encoding: Encoding): (IdTree, EventTree) = {
    val (idTree, enc) = decodeId(encoding)
    val (eventTree, _) = decodeEvents(enc)
    return (idTree, eventTree)
  }

  def decodeId(encoding: Encoding): (IdTree, Encoding) = {
    import causality.IdTree.{Branch, Leaf}
    encoding.get(2) match
      case 0 => encoding.del(2).get(1) match
        case 0 => (Leaf(0), encoding.del(3))
        case 1 => (Leaf(1), encoding.del(3))
        case _ => throw new Exception("error trying to decode id tree")
      case 1 => { val (i, r) = decodeId(encoding.del(2)); (Branch(Leaf(0), i), r) }
      case 2 => { val (i, r) = decodeId(encoding.del(2)); (Branch(i, Leaf(0)), r) }
      case 3 => { val (i1, r1) = decodeId(encoding.del(2)); val (i2, r2) = decodeId(r1); (Branch(i1, i2), r2) }
      case _ => throw new Exception("error trying to decode id tree")
  }

  def decodeEvents(encoding: Encoding): (EventTree, Encoding) = {
    import causality.EventTree.{Branch, Leaf}
    encoding.get(1) match
      case 0 => encoding.del(1).get(2) match
        case 0 => { val (e, r) = decodeEvents(encoding.del(3)); (Branch(0, Leaf(0), e), r) }
        case 1 => { val (e, r) = decodeEvents(encoding.del(3)); (Branch(0, e, Leaf(0)), r) }
        case 2 => { val (e1, r1) = decodeEvents(encoding.del(3)); val (e2, r2) = decodeEvents(r1); (Branch(0, e1, e2), r2) }
        case 3 => encoding.del(3).get(1) match
          case 0 => encoding.del(4).get(1) match
            case 0 => { val (n, r1) = decodeNum(encoding.del(5), 2); val (e, r2) = decodeEvents(r1); (Branch(n, Leaf(0), e), r2) }
            case 1 => { val (n, r1) = decodeNum(encoding.del(5), 2); val (e, r2) = decodeEvents(r1); (Branch(n, e, Leaf(0)), r2) }
            case _ => throw new Exception("error trying to decode event tree")
          case 1 => { val (n, r1) = decodeNum(encoding.del(4), 2); val (e1, r2) = decodeEvents(r1); val (e2, r3) = decodeEvents(r2); (Branch(n, e1, e2), r3) }
          case _ => throw new Exception("error trying to decode event tree")
        case _ => throw new Exception("error trying to decode event tree")
      case 1 => { val (n, e) = decodeNum(encoding.del(1), 2); (Leaf(n), e) }
      case _ => throw new Exception("error trying to decode event tree")
  }

  def decodeNum(encoding: Encoding, B: Int): (Int, Encoding) = {
    encoding.get(1) match
      case 0 => (encoding.del(1).get(B), encoding.del(1).del(B))
      case 1 => {
        val (n, enc) = decodeNum(encoding.del(1), B+1)
        (n + (1 << B), enc)
      }
  }
}
