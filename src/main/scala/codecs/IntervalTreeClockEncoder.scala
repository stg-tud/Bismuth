package de.tu_darmstadt.stg.daimpl
package codecs

import causality.{EventTree, IdTree, IntervalTreeClock}
import codecs.Encoder
import codecs.encoding.BitEncoding

import java.nio.ByteBuffer
import scala.annotation.targetName
import scala.math.pow

given IntervalTreeClockEncoder: Encoder[IntervalTreeClock] with {
  def encode(idTree: IdTree, eventTree: EventTree): BitEncoding =
    encodeId(idTree) + encodeEvents(eventTree)

  def decode(encoding: BitEncoding): (IdTree, EventTree) = {
    val (idTree, enc) = decodeId(encoding)
    val (eventTree, _) = decodeEvents(enc)
    (idTree, eventTree)
  }

  def encodeId(idTree: IdTree): BitEncoding = {
    import causality.IdTree.{Branch, Leaf}
    idTree match
      case Leaf(0)                  => BitEncoding().add(0, 2).add(0, 1)
      case Leaf(1)                  => BitEncoding().add(0, 2).add(1, 1)
      case Branch(Leaf(0), i      ) => BitEncoding().add(1, 2) + encodeId(i)
      case Branch(i      , Leaf(0)) => BitEncoding().add(2, 2) + encodeId(i)
      case Branch(i1     , i2     ) => BitEncoding().add(3, 2) + encodeId(i1) + encodeId(i2)
  }

  def decodeId(encoding: BitEncoding): (IdTree, BitEncoding) = {
    import causality.IdTree.{Branch, Leaf}
    encoding.get(2) match
      case 0 => encoding.del(2).get(1) match
        case 0 => (Leaf(0), encoding.del(3))
        case 1 => (Leaf(1), encoding.del(3))
        case _ => throw new Exception("error trying to decode id tree")
      case 1 => val (i, r) = decodeId(encoding.del(2)); (Branch(Leaf(0), i), r)
      case 2 => val (i, r) = decodeId(encoding.del(2)); (Branch(i, Leaf(0)), r)
      case 3 =>
        val (i1, r1) = decodeId(encoding.del(2))
        val (i2, r2) = decodeId(r1)
        (Branch(i1, i2), r2)
      case _ => throw new Exception("error trying to decode id tree")
  }

  def encodeEvents(eventTree: EventTree): BitEncoding = {
    import causality.EventTree.{Branch, Leaf}
    eventTree match
      case Branch(0, Leaf(0), e      ) => BitEncoding().add(0, 1).add(0, 2) + encodeEvents(e)
      case Branch(0, e      , Leaf(0)) => BitEncoding().add(0, 1).add(1, 2) + encodeEvents(e)
      case Branch(0, e1     , e2     ) => BitEncoding().add(0, 1).add(2, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Branch(n, Leaf(0), e      ) => BitEncoding().add(0, 1).add(3, 2).add(0, 1).add(0, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e      , Leaf(0)) => BitEncoding().add(0, 1).add(3, 2).add(0, 1).add(1, 1) + encodeNum(n, 2) + encodeEvents(e)
      case Branch(n, e1     , e2     ) => BitEncoding().add(0, 1).add(3, 2).add(1, 1) + encodeNum(n, 2) + encodeEvents(e1) + encodeEvents(e2)
      case Leaf(n)                     => BitEncoding().add(1, 1) + encodeNum(n, 2)
  }

  def decodeEvents(encoding: BitEncoding): (EventTree, BitEncoding) = {
    import causality.EventTree.{Branch, Leaf}
    require(encoding.digits > 0)
    encoding.get(1) match
      case 1 =>
        val (n, e) = decodeNum(encoding.del(1), 2)
        (Leaf(n), e)
      case 0 => encoding.get(3) match
        case 0 =>
          val (e, r) = decodeEvents(encoding.del(3))
          (Branch(0, Leaf(0), e), r)
        case 2 =>
          val (e, r) = decodeEvents(encoding.del(3))
          (Branch(0, e, Leaf(0)), r)
        case 4 =>
          val (e1, r1) = decodeEvents(encoding.del(3))
          val (e2, r2) = decodeEvents(r1)
          (Branch(0, e1, e2), r2)
        case 6 => encoding.get(4) match
          case 14 =>
            val (n, r1) = decodeNum(encoding.del(4), 2)
            val (e1, r2) = decodeEvents(r1)
            val (e2, r3) = decodeEvents(r2)
            (Branch(n, e1, e2), r3)
          case 6 => encoding.get(5) match
            case 6 =>
              val (n, r1) = decodeNum(encoding.del(5), 2)
              val (e, r2) = decodeEvents(r1)
              (Branch(n, Leaf(0), e), r2)
            case 22 =>
              val (n, r1) = decodeNum(encoding.del(5), 2)
              val (e, r2) = decodeEvents(r1)
              (Branch(n, e, Leaf(0)), r2)
  }

  def encodeNum(n: Int, B: Int): BitEncoding = {
    if (n < pow(2, B)) {
      BitEncoding().add(0, 1).add(n, B)
    } else {
      BitEncoding().add(1, 1) + encodeNum(n - (1 << B), B+1)
    }
  }

  def decodeNum(encoding: BitEncoding, B: Int): (Int, BitEncoding) = {
    require(encoding.digits > 0)
    encoding.get(1) match
      case 0 => (encoding.del(1).get(B), encoding.del(1).del(B))
      case 1 =>
        val (n, enc) = decodeNum(encoding.del(1), B+1)
        (n + (1 << B), enc)
  }

  override def write(obj: IntervalTreeClock, buffer: ByteBuffer): Unit = writeArray(obj).toBuffer
  override def writeArray(obj: IntervalTreeClock): Array[Byte] = encode(obj.idTree, obj.eventTree).toByteArray
  override def writeString(obj: IntervalTreeClock): String = encode(obj.idTree, obj.eventTree).toString

  override def read(buffer: ByteBuffer, length: Int): IntervalTreeClock = {
    val bytes: Array[Byte] = new Array[Byte](length)
    buffer.get(bytes)
    readArray(bytes)
  }

  override def readArray(bytes: Array[Byte]): IntervalTreeClock = {
    val encoding = BitEncoding.fromBytes(bytes)
    val (idTree, eventTree) = decode(encoding)
    IntervalTreeClock(idTree, eventTree)
  }

  override def readString(bytes: String): IntervalTreeClock = {
    val encoding = BitEncoding.fromString(bytes)
    val (idTree, eventTree) = decode(encoding)
    IntervalTreeClock(idTree, eventTree)
  }
}
