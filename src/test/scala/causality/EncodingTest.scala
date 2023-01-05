package de.tu_darmstadt.stg.daimpl
package causality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class EncodingTest extends AnyFlatSpec {
  "toString" should "work for Encoding" in {
    assert(Encoding().addBits(0, 1).toString() == "0")
    assert(Encoding().addBits(1, 1).toString() == "1")
    assert(Encoding().addBits(2, 3).toString() == "010")
    assert(Encoding().addBits(1, 2).addBits(2, 2).toString() == "0110")
  }

  "add" should "work for Encoding" in {
    assert(Encoding().addBits(0, 1) + Encoding().addBits(1, 1) == Encoding().addBits(0, 1).addBits(1, 1))
    assert(Encoding().addBits(0, 1).addBits(1, 1) + Encoding().addBits(0, 1) == Encoding().addBits(0, 1).addBits(1, 1).addBits(0, 1))
    assert(Encoding().addBits(0, 1).addBits(1, 1) + Encoding().addBits(1, 1).addBits(0, 1) == Encoding().addBits(0, 1).addBits(1, 1).addBits(1, 1).addBits(0, 1))
  }

  it should "work for empty encodings" in {
    assert(Encoding() + Encoding() == Encoding())
  }

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

  "encodeNum" should "encode 0" in {
    assert(Encoder.encodeNum(0, 1).toString() == "00")
    assert(Encoder.encodeNum(0, 3).toString() == "0000")
  }

  it should "encode 1" in {
    assert(Encoder.encodeNum(1, 1).toString() == "01")
    assert(Encoder.encodeNum(1, 3).toString() == "0001")
  }

  it should "encode 42" in {
    assert(Encoder.encodeNum(42, 2).toString() == "111001110")
  }

  it should "work with enough digits" in {
    assert(Encoder.encodeNum(10, 4).toString() == "01010")
  }

  it should "work with few digits" in {
    assert(Encoder.encodeNum(10, 2).toString() == "10110")
  }

  "encode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(Encoder.encode(idTree, eventTree).toString() == "0001000")
  }
}
