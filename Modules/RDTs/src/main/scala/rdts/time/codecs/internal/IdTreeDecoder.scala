package com.github.ckuessner
package codecs.internal

import causality.IdTree

private[codecs] object IdTreeDecoder {
  def decode(byteArray: Array[Byte]): IdTree = {
    decode(BitReader(byteArray))
  }


  @throws[MatchError]
  def decode(bitReader: BitReader): IdTree = {
    bitReader.readNextTwoBits() match {
      case 0 =>
        bitReader.readNextBit() match {
          case 0 => IdTree.Leaf(0) // enc(0) = <<0:2, 0:1>>
          case 1 => IdTree.Leaf(1) // enc(1) = <<0:2, 1:1>>
        }
      case 1 => // enc((0,ir)) = <<1:2, enc(ir)>>
        IdTree.Branch(IdTree.Leaf(0), decode(bitReader))
      case 2 => // enc((il, 0)) = <<2:2, enc(il)>>
        IdTree.Branch(decode(bitReader), IdTree.Leaf(0))
      case 3 => // enc((il, ir)) = <<3:2, enc(il), enc(ir)>>
        IdTree.Branch(decode(bitReader), decode(bitReader))
    }
  }
}
