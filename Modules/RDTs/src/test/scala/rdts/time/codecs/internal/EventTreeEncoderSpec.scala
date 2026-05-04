package com.github.ckuessner
package codecs.internal

import causality.EventTree
import causality.EventTree.{Branch, Leaf}

import munit.FunSuite

import scala.language.implicitConversions

class EventTreeEncoderSpec extends FunSuite {

  test("encode is the inverse of decode for Leafs") {
    decodeIsInverseOfEncode(Leaf(0))
    decodeIsInverseOfEncode(Leaf(1))
    decodeIsInverseOfEncode(Leaf(42))
    decodeIsInverseOfEncode(Leaf(Int.MaxValue))
  }

  test("encode is the inverse of decode for Branches with n=0") {
    decodeIsInverseOfEncode(Branch(0, 0, 42))
    decodeIsInverseOfEncode(Branch(0, Int.MaxValue, 0))
    decodeIsInverseOfEncode(Branch(0, 0, 64))
  }

  test("encode is the inverse of decode for Branches with n!=0") {
    decodeIsInverseOfEncode(Branch(1, 0, 41))
    decodeIsInverseOfEncode(Branch(42, 21, 0))
    decodeIsInverseOfEncode(Branch(Int.MaxValue, 64, 0))
    decodeIsInverseOfEncode(Branch(1, Int.MaxValue, 0))
  }

  private def decodeIsInverseOfEncode(eventTree: EventTree): Unit = {
    val encoded = EventTreeEncoder.encode(eventTree).toByteArray
    val decoded = EventTreeDecoder.decode(encoded)
    assertEquals(decoded, eventTree)
  }
}
