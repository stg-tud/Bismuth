package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf, anonymous, seed, given}

import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class IdTreeTest extends AnyFlatSpec {
  "split" should "work for seed" in {
    assert(seed.split == (Branch(1, 0), Branch(0, 1)))
  }

  it should "work on anonymous stamps" in {
    assert(anonymous.split == (Leaf(0), Leaf(0)))
  }

  it should "work for left hand of branch" in {
    val firstSplit = seed.split._1
    val secondSplit = firstSplit.split
    assert(secondSplit._1 == Branch(Branch(1, 0), 0))
    assert(secondSplit._2 == Branch(Branch(0, 1), 0))
  }

  it should "work for right hand of branch" in {
    val firstSplit = seed.split._2
    val secondSplit = firstSplit.split
    assert(secondSplit._1 == Branch(0, Branch(1, 0)))
    assert(secondSplit._2 == Branch(0, Branch(0, 1)))
  }

  it should "descend into right hand if left hand is 0" in {
    val id = Branch(0, 1)
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(0, Branch(1, 0)))
    assert(secondSplit == Branch(0, Branch(0, 1)))
  }

  it should "descend into left hand if right hand is 0" in {
    val id = Branch(1, 0)
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(Branch(1, 0), 0))
    assert(secondSplit == Branch(Branch(0, 1), 0))
  }

  it should "work for branches that don't have a 0 on either side" in {
    val id = Branch(1, Branch(1, 0))
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(1, 0))
    assert(secondSplit == Branch(0, Branch(1, 0)))
  }

  "sum" should "return the inverse of split for seed" in {
    assert((seed.split._1 + seed.split._2).contains(seed))
  }

  it should "return 0 for (0,0)" in {
    assert((anonymous + anonymous).contains(Leaf(0)))
  }

  it should "return None if adding two overlapping ids" in {
    assert((seed + seed).isEmpty)
    assert((Branch(1, 0) + Branch(1, 0)).isEmpty)
    assert((Branch(0, 1) + Branch(0, 1)).isEmpty)
  }

  it should "nomalize id" in {
    assert((Branch(1, Branch(1, 0)) + Branch(0, Branch(0, 1))).contains(seed))
  }

  "NormalForm[IdTree]" should "normalize to 1 if Tree is all 1s" in {
    assert(Leaf(1).normalize == Leaf(1))
    assert(Branch(1, 1).normalize == Leaf(1))
    assert(Branch(Branch(1, 1), 1).normalize == Leaf(1))
    assert(Branch(1, Branch(1, 1)).normalize == Leaf(1))
    assert(Branch(1, Branch(Branch(1, 1), 1)).normalize == Leaf(1))
  }

  it should "normalize to 0 if Tree is all 0s" in {
    assert(Leaf(0).normalize == Leaf(0))
    assert(Branch(0, 0).normalize == Leaf(0))
    assert(Branch(Branch(0, 0), 0).normalize == Leaf(0))
    assert(Branch(0, Branch(0, 0)).normalize == Leaf(0))
    assert(Branch(0, Branch(Branch(0, 0), 0)).normalize == Leaf(0))
  }

  it should "normalize nested id tree correctly" in {
    val id = Branch(Branch(1, 1), Branch(Branch(0, Branch(1, Branch(1, 1))), 0))
    assert(id.normalize == Branch(1, Branch(Branch(0, 1), 0)))
  }

}
