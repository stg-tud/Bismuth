package de.tu_darmstadt.stg.daimpl
package causality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class EncodingTest extends AnyFlatSpec {
  "fromString" should "work for Encoding" in {
    assert(Encoding.fromString("0") == Encoding().add(0, 1))
    assert(Encoding.fromString("1") == Encoding().add(1, 1))
    assert(Encoding.fromString("010") == Encoding().add(2, 3))
    assert(Encoding.fromString("0110") == Encoding().add(1, 2).add(2, 2))
  }

  "toString" should "work for Encoding" in {
    assert(Encoding().add(0, 1).toString() == "0")
    assert(Encoding().add(1, 1).toString() == "1")
    assert(Encoding().add(2, 3).toString() == "010")
    assert(Encoding().add(1, 2).add(2, 2).toString() == "0110")
  }

  "add" should "work for Encoding" in {
    assert(Encoding().add(0, 1) + Encoding().add(1, 1) == Encoding().add(0, 1).add(1, 1))
    assert(Encoding().add(0, 1).add(1, 1) + Encoding().add(0, 1) == Encoding().add(0, 1).add(1, 1).add(0, 1))
    assert(Encoding().add(0, 1).add(1, 1) + Encoding().add(1, 1).add(0, 1) == Encoding().add(0, 1).add(1, 1).add(1, 1).add(0, 1))
  }

  it should "work for empty encodings" in {
    assert(Encoding() + Encoding() == Encoding())
  }

  "get" should "work for Encoding" in {
    val enc = Encoding.fromString("01001")
    assert(enc.get(2) == 1)
    assert(enc.get(3) == 2)
    assert(enc.get(5) == 9)
  }

  "del" should "work for Encoding" in {
    assert(Encoding.fromString("01001").del(1) == Encoding.fromString("1001"))
    assert(Encoding.fromString("01001").del(2) == Encoding.fromString("001"))
    assert(Encoding.fromString("01001").del(4) == Encoding.fromString("1"))
  }



  /***************
   *** ENCODER ***
   ***************/

  "encodeId" should "encode Leaf(0)" in {
    assert(Encoder.encodeId(IdTree.Leaf(0)).toString() == "000")
  }

  it should "encode Leaf(1)" in {
    assert(Encoder.encodeId(IdTree.Leaf(1)).toString() == "001")
  }

  it should "encode right subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1))).toString() == "01001")
  }

  it should "encode left subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0))).toString() == "10001")
  }

  it should "encode both subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1))).toString() == "11001001")
  }

  "encodeEvents" should "encode Leaf(0)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(0)).toString() == "1000")
  }

  it should "encode Leaf(1)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(1)).toString() == "1001")
  }

  it should "encode Leaf(42)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(42)).toString() == "1111001110")
  }

  it should "encode right subtree when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1))).toString() == "0001001")
  }

  it should "encode left subtree when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0))).toString() == "0011001")
  }

  it should "encode both subtrees when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1))).toString() == "01010011001")
  }

  it should "encode right subtree" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1))).toString() == "011000011001")
  }

  it should "encode left subtree" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0))).toString() == "011010011001")
  }

  it should "encode both subtrees" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1))).toString() == "011100110011001")
  }

  "encodeNum" should "work" in {
    assert(Encoder.encodeNum(0, 2).toString() == "000")
    assert(Encoder.encodeNum(1, 2).toString() == "001")
    assert(Encoder.encodeNum(42, 2).toString() == "111001110")
  }

  "encode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(Encoder.encode(idTree, eventTree).toString() == "0001000")
  }


  /***************
   *** DECODER ***
   ***************/

  "decodeId" should "decode Leaf(0)" in {
    assert(Decoder.decodeId(Encoding.fromString("000"))._1 == IdTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(Decoder.decodeId(Encoding.fromString("001"))._1 == IdTree.Leaf(1))
  }

  it should "decode right subtree" in {
    assert(Decoder.decodeId(Encoding.fromString("01001"))._1 == IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(Decoder.decodeId(Encoding.fromString("10001"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0)))
  }

  it should "decode both subtree" in {
    assert(Decoder.decodeId(Encoding.fromString("11001001"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1)))
  }

  "decodeEvents" should "decode Leaf(0)" in {
    assert(Decoder.decodeEvents(Encoding.fromString("1000"))._1 == EventTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(Decoder.decodeEvents(Encoding.fromString("1001"))._1 == EventTree.Leaf(1))
  }

  it should "decode Leaf(42)" in {
    assert(Decoder.decodeEvents(Encoding.fromString("1111001110"))._1 == EventTree.Leaf(42))
  }

  it should "decode right subtree when base is 0" in {
    assert(Decoder.decodeEvents(Encoding.fromString("0001001"))._1 == EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree when base is 0" in {
    assert(Decoder.decodeEvents(Encoding.fromString("0011001"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees when base is 0" in {
    assert(Decoder.decodeEvents(Encoding.fromString("01010011001"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  it should "decode right subtree" in {
    assert(Decoder.decodeEvents(Encoding.fromString("011000011001"))._1 == EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(Decoder.decodeEvents(Encoding.fromString("011010011001"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees" in {
    assert(Decoder.decodeEvents(Encoding.fromString("011100110011001"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  "decodeNum" should "work" in {
    assert(Decoder.decodeNum(Encoding.fromString("000"), 2)._1 == 0)
    assert(Decoder.decodeNum(Encoding.fromString("001"), 2)._1 == 1)
    assert(Decoder.decodeNum(Encoding.fromString("111001110"), 2)._1 == 42)
  }

  "decode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(Decoder.decode(Encoding.fromString("0001000")) == (idTree, eventTree))
  }
}
