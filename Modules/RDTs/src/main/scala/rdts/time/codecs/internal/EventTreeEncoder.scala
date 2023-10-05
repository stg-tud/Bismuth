package com.github.ckuessner
package codecs.internal

import causality.EventTree

private[codecs] object EventTreeEncoder {
  def encode(eventTree: EventTree): BitWriter = {
    val bitWriter = BitWriter.empty
    encode(eventTree, bitWriter)
    bitWriter
  }

  def encode(eventTree: EventTree, bitWriter: BitWriter): Unit = eventTree match
    case EventTree.Leaf(n) =>
      encode(n, bitWriter) // Encoding a leaf is the same as encoding a number
    case EventTree.Branch(0, EventTree.Leaf(0), er) => // enc((0, 0, er)) = <<0:1, 0:2, enc(er)>>
      bitWriter.write(0, 3) // <<0:1, 0:2>> = 0b000 = 0:3
      encode(er, bitWriter)
    case EventTree.Branch(0, el, EventTree.Leaf(0)) => // enc((0, el , 0)) = <<0:1, 1:2, enc(el)>>
      bitWriter.write(1, 3) // <<0:1, 1:2>> = 0b001 = 1:3
      encode(el, bitWriter)
    case EventTree.Branch(0, el, er) => // enc((0,el,er)) = <<0:1, 2:2, enc(el), enc(er)>>
      bitWriter.write(2, 3) // <<0:1, 2:2>> = 0b010 = 2:3
      encode(el, bitWriter)
      encode(er, bitWriter)
    case EventTree.Branch(n, EventTree.Leaf(0), er) => // enc((n, 0, er)) = <<0:1, 3:2, 0:1, 0:1, enc(n), enc(er)>>
      bitWriter.write(12, 5) // <<0:1, 3:2, 0:1, 0:1>> = 0b01100 = 12:5
      encode(n, bitWriter)
      encode(er, bitWriter)
    case EventTree.Branch(n, el, EventTree.Leaf(0)) => // enc((n, el, 0)) = <<0:1, 3:2, 0:1, 1:1, enc(n), enc(el)>>
      bitWriter.write(13, 5) // <<0:1, 3:2, 0:1, 1:1>> = 0b01101 = 13:5
      encode(n, bitWriter)
      encode(el, bitWriter)
    case EventTree.Branch(n, el, er) => // enc((n, el, er)) = <<0:1, 3:2, 1:1, enc(n), enc(el), enc(er)>>
      bitWriter.write(7, 4) // <<0:1, 3:2, 1:1>> = 0b0111 = 7:4
      encode(n, bitWriter)
      encode(el, bitWriter)
      encode(er, bitWriter)

  private def encode(number: Int, bitWriter: BitWriter): Unit = {
    var bits        = 2
    var numToEncode = number
    // TODO: Test the impact on performance of this size-optimization (from the paper)
    while (numToEncode >= (1L << bits)) {
      numToEncode -= (1 << bits)
      bits += 1
    }
    bitWriter.write((1 << bits) - 2, bits)
    bitWriter.write(numToEncode, bits)
  }
}
