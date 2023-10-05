package com.github.ckuessner
package codecs.internal

import causality.EventTree
import causality.EventTree.{Branch, Leaf}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions

class EventTreeEncoderSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  "encode" should "be the inverse of decode for Leafs" in {
    decodeIsInverseOfEncode(Leaf(0))
    decodeIsInverseOfEncode(Leaf(1))
    decodeIsInverseOfEncode(Leaf(42))
    decodeIsInverseOfEncode(Leaf(Int.MaxValue))
  }

  it should "be the inverse of decode for Branches with n=0" in {
    decodeIsInverseOfEncode(Branch(0, 0, 42))
    decodeIsInverseOfEncode(Branch(0, Int.MaxValue, 0))
    decodeIsInverseOfEncode(Branch(0, 0, 64))
  }

  it should "be the inverse of decode for Branches with n!=0" in {
    decodeIsInverseOfEncode(Branch(1, 0, 41))
    decodeIsInverseOfEncode(Branch(42, 21, 0))
    decodeIsInverseOfEncode(Branch(Int.MaxValue, 64, 0))
    decodeIsInverseOfEncode(Branch(1, Int.MaxValue, 0))
  }

  private def decodeIsInverseOfEncode(eventTree: EventTree): Unit = {
    val encoded = EventTreeEncoder.encode(eventTree).toByteArray
    val decoded = EventTreeDecoder.decode(encoded)
    decoded shouldBe eventTree
  }

}
