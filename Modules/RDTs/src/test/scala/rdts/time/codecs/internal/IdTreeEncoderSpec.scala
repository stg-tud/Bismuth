package com.github.ckuessner
package codecs.internal

import causality.IdTree
import causality.IdTree.{Branch, Leaf}

import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IdTreeEncoderSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "encode" should "be the inverse of decode for Leafs" in {
    decodeIsInverseOfEncode(Leaf(0))
    decodeIsInverseOfEncode(Leaf(1))
  }

  it should "be the inverse of decode for Branches" in {
    decodeIsInverseOfEncode(Branch(Leaf(0), Leaf(1)))
    decodeIsInverseOfEncode(Branch(Leaf(1), Leaf(0)))
    decodeIsInverseOfEncode(Branch(Leaf(1), Branch(Leaf(0), Leaf(1))))
    decodeIsInverseOfEncode(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(0), Leaf(1))))
  }

  it should "be the inverse of decode for non-normalized Branches" in {
    decodeIsInverseOfEncode(Branch(Leaf(1), Leaf(1)))
    decodeIsInverseOfEncode(Branch(Leaf(0), Leaf(0)))
    decodeIsInverseOfEncode(Branch(Leaf(0), Branch(Leaf(0), Leaf(1))))
    decodeIsInverseOfEncode(Branch(Branch(Leaf(0), Leaf(1)), Leaf(0)))
  }

  it should "be the inverse of decode for deeply nested id" in {
    var id = IdTree.seed
    (0 until 1_000).foreach(_ => id = id.split._1)

    decodeIsInverseOfEncode(id)
  }

  private def decodeIsInverseOfEncode(idTree: IdTree): Unit = {
    val encoded = IdTreeEncoder.encode(idTree).toByteArray
    val decoded = IdTreeDecoder.decode(encoded)
    decoded shouldBe idTree
  }
}
