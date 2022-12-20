package de.tu_darmstadt.stg.daimpl
package causality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class EncodingTest extends AnyFlatSpec {
  "toBits" should "work for Digit" in {
    assert(Digit(0, 1).toBits() == "0")
    assert(Digit(1, 1).toBits() == "1")
    assert(Digit(2, 3).toBits() == "010")
  }

  it should "work for Encoding" in {
    assert(Encoding(Digit(1, 2), Digit(2, 2)).toBits() == "0110")
  }

  "add" should "work for two digits" in {
    assert(Encoding(Digit(0, 1)) + Encoding(Digit(1, 1)) == Encoding(Digit(0, 1), Digit(1, 1)))
  }

  it should "work for more digits" in {
    assert(Encoding(Digit(0, 1), Digit(1, 1)) + Encoding(Digit(0, 1)) == Encoding(Digit(0, 1), Digit(1, 1), Digit(0, 1)))
    assert(Encoding(Digit(0, 1), Digit(1, 1)) + Encoding(Digit(1, 1), Digit(0, 1)) == Encoding(Digit(0, 1), Digit(1, 1), Digit(1, 1), Digit(0, 1)))
  }

  it should "work for empty encodings" in {
    assert(Encoding() + Encoding() == Encoding())
  }

  "encodeId" should "encode Leaf(0)" in {
    assert(Encoder.encodeId(IdTree.Leaf(0)).toBits() == "000")
  }

  it should "encode Leaf(1)" in {
    assert(Encoder.encodeId(IdTree.Leaf(1)).toBits() == "001")
  }

  it should "encode right subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1))).toBits() == "01001")
  }

  it should "encode left subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0))).toBits() == "10001")
  }

  it should "encode both subtree" in {
    assert(Encoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1))).toBits() == "11001001")
  }

  "encodeEvents" should "encode Leaf(0)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(0)).toBits() == "1000")
  }

  it should "encode Leaf(1)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(1)).toBits() == "1001")
  }

  it should "encode Leaf(42)" in {
    assert(Encoder.encodeEvents(EventTree.Leaf(42)).toBits() == "1111001110")
  }

  it should "encode right subtree when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1))).toBits() == "0001001")
  }

  it should "encode left subtree when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0))).toBits() == "0011001")
  }

  it should "encode both subtrees when base is 0" in {
    assert(Encoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1))).toBits() == "01010011001")
  }

  it should "encode right subtree" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1))).toBits() == "011000011001")
  }

  it should "encode left subtree" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0))).toBits() == "011010011001")
  }

  it should "encode both subtrees" in {
    assert(Encoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1))).toBits() == "011100110011001")
  }

  "encodeNum" should "encode 0" in {
    assert(Encoder.encodeNum(0, 1).toBits() == "00")
    assert(Encoder.encodeNum(0, 3).toBits() == "0000")
  }

  it should "encode 1" in {
    assert(Encoder.encodeNum(1, 1).toBits() == "01")
    assert(Encoder.encodeNum(1, 3).toBits() == "0001")
  }

  it should "encode 42" in {
    assert(Encoder.encodeNum(42, 2).toBits() == "111001110")
  }

  it should "work with enough digits" in {
    assert(Encoder.encodeNum(10, 4).toBits() == "01010")
  }

  it should "work with few digits" in {
    assert(Encoder.encodeNum(10, 2).toBits() == "10110")
  }

  "encode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(Encoder.encode(idTree, eventTree).toBits() == "0001000")
  }
}
