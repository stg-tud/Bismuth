package com.github.ckuessner
package codecs.internal

import causality.IdTree
import causality.IdTree.{Branch, Leaf}

import munit.FunSuite

class IdTreeEncoderSpec extends FunSuite {

  test("encode is the inverse of decode for Leafs") {
    decodeIsInverseOfEncode(Leaf(0))
    decodeIsInverseOfEncode(Leaf(1))
  }

  test("encode is the inverse of decode for Branches") {
    decodeIsInverseOfEncode(Branch(Leaf(0), Leaf(1)))
    decodeIsInverseOfEncode(Branch(Leaf(1), Leaf(0)))
    decodeIsInverseOfEncode(Branch(Leaf(1), Branch(Leaf(0), Leaf(1))))
    decodeIsInverseOfEncode(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(0), Leaf(1))))
  }

  test("encode is the inverse of decode for non-normalized Branches") {
    decodeIsInverseOfEncode(Branch(Leaf(1), Leaf(1)))
    decodeIsInverseOfEncode(Branch(Leaf(0), Leaf(0)))
    decodeIsInverseOfEncode(Branch(Leaf(0), Branch(Leaf(0), Leaf(1))))
    decodeIsInverseOfEncode(Branch(Branch(Leaf(0), Leaf(1)), Leaf(0)))
  }

  test("encode is the inverse of decode for deeply nested id") {
    var id = IdTree.seed
    (0 until 1_000).foreach(_ => id = id.split._1)

    decodeIsInverseOfEncode(id)
  }

  private def decodeIsInverseOfEncode(idTree: IdTree): Unit = {
    val encoded = IdTreeEncoder.encode(idTree).toByteArray
    val decoded = IdTreeDecoder.decode(encoded)
    assertEquals(decoded, idTree)
  }
}
