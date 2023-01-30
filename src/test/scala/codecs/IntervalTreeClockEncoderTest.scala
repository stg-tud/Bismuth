package de.tu_darmstadt.stg.daimpl
package codecs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import de.tu_darmstadt.stg.daimpl.causality.IntervalTreeClock
import de.tu_darmstadt.stg.daimpl.causality.IdTree
import de.tu_darmstadt.stg.daimpl.causality.EventTree

import de.tu_darmstadt.stg.daimpl.causality.IntervalTreeClockGenerators.genIntervalTreeClock

class EncodingTest extends AnyFlatSpec {
  "fromString" should "work for Encoding" in {
    assert(Encoding.fromString("0") == Encoding().add(0, 1))
    assert(Encoding.fromString("1") == Encoding().add(1, 1))
    assert(Encoding.fromString("010") == Encoding().add(2, 3))
    assert(Encoding.fromString("1001") == Encoding().add(1, 2).add(2, 2))
  }

  "toString" should "work for Encoding" in {
    assert(Encoding().add(0, 1).toString() == "0")
    assert(Encoding().add(1, 1).toString() == "1")
    assert(Encoding().add(2, 3).toString() == "010")
    assert(Encoding().add(1, 2).add(2, 2).toString() == "1001")
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
    val enc = Encoding.fromString("10010")
    assert(enc.get(2) == 2)
    assert(enc.get(3) == 2)
    assert(enc.get(5) == 18)
  }

  "del" should "work for Encoding" in {
    assert(Encoding.fromString("10010").del(1) == Encoding.fromString("1001"))
    assert(Encoding.fromString("10010").del(2) == Encoding.fromString("100"))
    assert(Encoding.fromString("10010").del(4) == Encoding.fromString("1"))
  }
}


class IntervalTreeClockEncoderTest extends EncoderSpec[IntervalTreeClock] {

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
    assert(IntervalTreeClockEncoder.decodeId(Encoding.fromString("000"))._1 == IdTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.decodeId(Encoding.fromString("100"))._1 == IdTree.Leaf(1))
  }

  it should "decode right subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(Encoding.fromString("10001"))._1 == IdTree.Branch(IdTree.Leaf(0), IdTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(Encoding.fromString("10010"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(0)))
  }

  it should "decode both subtree" in {
    assert(IntervalTreeClockEncoder.decodeId(Encoding.fromString("10010011"))._1 == IdTree.Branch(IdTree.Leaf(1), IdTree.Leaf(1)))
  }

  "decodeEvents" should "decode Leaf(0)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("0001"))._1 == EventTree.Leaf(0))
  }

  it should "decode Leaf(1)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("0101"))._1 == EventTree.Leaf(1))
  }

  it should "decode Leaf(42)" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("0111001111"))._1 == EventTree.Leaf(42))
  }

  it should "decode right subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("0101000"))._1 == EventTree.Branch(0, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("0101010"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees when base is 0" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("01010101100"))._1 == EventTree.Branch(0, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  it should "decode right subtree" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("010101000110"))._1 == EventTree.Branch(1, EventTree.Leaf(0), EventTree.Leaf(1)))
  }

  it should "decode left subtree" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("010101010110"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(0)))
  }

  it should "decode both subtrees" in {
    assert(IntervalTreeClockEncoder.decodeEvents(Encoding.fromString("010101010101110"))._1 == EventTree.Branch(1, EventTree.Leaf(1), EventTree.Leaf(1)))
  }

  "decodeNum" should "work" in {
    assert(IntervalTreeClockEncoder.decodeNum(Encoding.fromString("000"), 2)._1 == 0)
    assert(IntervalTreeClockEncoder.decodeNum(Encoding.fromString("010"), 2)._1 == 1)
    assert(IntervalTreeClockEncoder.decodeNum(Encoding.fromString("011100111"), 2)._1 == 42)
  }

  "decode" should "work for itc with zeros" in {
    val idTree = IdTree.Leaf(0)
    val eventTree = EventTree.Leaf(0)
    assert(IntervalTreeClockEncoder.decode(Encoding.fromString("0001000")) == (idTree, eventTree))
  }
}
