package de.tu_darmstadt.stg.daimpl
package codecs

import causality.IntervalTreeClockGenerators.genIntervalTreeClock
import causality.{EventTree, IdTree, IntervalTreeClock}
import codecs.encoding.BitEncoding

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class IntervalTreeClockEncoderTest extends EncoderSpec[IntervalTreeClock](using IntervalTreeClockEncoder, genIntervalTreeClock) {

  /***************
   *** ENCODER ***
   ***************/

  "encodeId" should "encode Leaf(0)" in {
    assert(IntervalTreeClockEncoder.encodeId(IdTree.Leaf(0)).toString() == "000")
  }

  it should "encode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.encodeId(IdTree.Leaf(1)).toString() == "100")
  }

  it should "encode right subtree" in {
    assert(IntervalTreeClockEncoder.encodeId(IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1))).toString() == "10001")
  }

  it should "encode left subtree" in {
    assert(IntervalTreeClockEncoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0))).toString() == "10010")
  }

  it should "encode both subtree" in {
    assert(IntervalTreeClockEncoder.encodeId(IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1))).toString() == "10010011")
  }

  "encodeEvents" should "encode Leaf(0)" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Leaf(0)).toString() == "0001")
  }

  it should "encode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Leaf(1)).toString() == "0101")
  }

  it should "encode Leaf(42)" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Leaf(42)).toString() == "0111001111")
  }

  it should "encode right subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1))).toString() == "0101000")
  }

  it should "encode left subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0))).toString() == "0101010")
  }

  it should "encode both subtrees when base is 0" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1))).toString() == "01010101100")
  }

  it should "encode right subtree" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1))).toString() == "010101000110")
  }

  it should "encode left subtree" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0))).toString() == "010101010110")
  }

  it should "encode both subtrees" in {
    assert(IntervalTreeClockEncoder.encodeEvents(EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1))).toString() == "010101010101110")
  }

  "encodeNum" should "work" in {
    assert(IntervalTreeClockEncoder.encodeNum(0, 2).toString() == "000")
    assert(IntervalTreeClockEncoder.encodeNum(1, 2).toString() == "010")
    assert(IntervalTreeClockEncoder.encodeNum(42, 2).toString() == "011100111")
  }

  "encode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(IntervalTreeClockEncoder.encode(idTree, eventTree).toString() == "0001000")
  }


  /***************
   *** DECODER ***
   ***************/

  "decodeId" should "decode Leaf(0)" in {
    assert(IntervalTreeClockEncoder.decodeId(BitEncoding.fromString("000"))._1 == IdTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.decodeId(BitEncoding.fromString("100"))._1 == IdTree.Leaf(1))
  }

  it should "decode right subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(BitEncoding.fromString("10001"))._1 == IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(BitEncoding.fromString("10010"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0)))
  }

  it should "decode both subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(BitEncoding.fromString("10010011"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1)))
  }

  "decodeEvents" should "decode Leaf(0)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("0001"))._1 == EventTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("0101"))._1 == EventTree.Leaf(1))
  }

  it should "decode Leaf(42)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("0111001111"))._1 == EventTree.Leaf(42))
  }

  it should "decode right subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("0101000"))._1 == EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("0101010"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("01010101100"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  it should "decode right subtree" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("010101000110"))._1 == EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("010101010110"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees" in {
    assert(IntervalTreeClockEncoder.decodeEvents(BitEncoding.fromString("010101010101110"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  "decodeNum" should "work" in {
    assert(IntervalTreeClockEncoder.decodeNum(BitEncoding.fromString("000"), 2)._1 == 0)
    assert(IntervalTreeClockEncoder.decodeNum(BitEncoding.fromString("010"), 2)._1 == 1)
    assert(IntervalTreeClockEncoder.decodeNum(BitEncoding.fromString("011100111"), 2)._1 == 42)
  }

  "decode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(IntervalTreeClockEncoder.decode(BitEncoding.fromString("0001000")) == (idTree, eventTree))
  }
}
