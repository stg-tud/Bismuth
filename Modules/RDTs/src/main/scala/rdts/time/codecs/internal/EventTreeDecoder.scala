package com.github.ckuessner
package codecs.internal

import causality.EventTree

import scala.language.implicitConversions

private[codecs] object EventTreeDecoder {
  def decode(byteArray: Array[Byte]): EventTree = {
    decode(BitReader(byteArray))
  }

  @throws[DecoderException]
  def decode(bitReader: BitReader): EventTree = {
    bitReader.readNextBit() match
      case 0 => { // Everything starting with a 0 is a Branch
        bitReader.readNextTwoBits() match {
          case 0 => // enc((0, 0, er)) = <<0:1, 0:2, enc(er)>>
            val er = decode(bitReader)
            EventTree.Branch(0, 0, er)
          case 1 => // enc((0, el , 0)) = <<0:1, 1:2, enc(el)>>
            val el = decode(bitReader)
            EventTree.Branch(0, el, 0)
          case 2 => // enc((0,el,er)) = <<0:1, 2:2, enc(el), enc(er)>>
            val el = decode(bitReader)
            val er = decode(bitReader)
            EventTree.Branch(0, el, er)
          case 3 =>
            bitReader.readNextTwoBits() match {
              case 0 => // enc((n, 0, er)) = <<0:1, 3:2, 0:1, 0:1, enc(n), enc(er)>>
                // We haven't consumed the leading 1 of enc(n), therefore decode it as a Leaf
                decode(bitReader) match
                  case EventTree.Leaf(n) => EventTree.Branch(n, 0, decode(bitReader))
                  case _                 => throw new DecoderException("Unexpected bit while decoding a number")
              case 1 => // enc((n, el, 0)) = <<0:1, 3:2, 0:1, 1:1, enc(n), enc(el)>>
                // We haven't consumed the leading 1 of enc(n), therefore decode it as a Leaf
                decode(bitReader) match
                  case EventTree.Leaf(n) => EventTree.Branch(n, decode(bitReader), 0)
                  case _                 => throw new DecoderException("Unexpected bit while decoding a number")
              case 3 => // enc((n, el, er)) = <<0:1, 3:2, 1:1, enc(n), enc(el), enc(er)>>
                // In this case, we _have_ already consumed the leading 1 of the enc(n), therefore we read a number, not a leaf
                val n  = decodeNum(bitReader)
                val el = decode(bitReader)
                val er = decode(bitReader)
                EventTree.Branch(n, el, er)
            }
        }
      }
      case 1 => // Everything starting with a 1 is a Leaf
        EventTree.Leaf(decodeNum(bitReader))
  }

  private def decodeNum(reader: BitReader): Int = {
    // We assume that the leading 1 was already consumed, therefore add 1 to number of bits to next zero (inclusive)
    val bits    = 1 + reader.readToNextZeroAndReturnNumberOfBitsRead()
    val readNum = reader.readNextIntOfLength(bits)
    // Since we subtracted 2^2..2^(bits-1), add them back
    readNum + (1 << bits) - 1 - 3
  }
}
