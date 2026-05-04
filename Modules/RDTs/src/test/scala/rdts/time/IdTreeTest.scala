package com.github.ckuessner
package causality

import causality.IdTree.{Branch, Leaf, anonymous, seed, given}
import causality.IdTreeGenerators.{genIdTree, genIdTreeBySplitting, genIdTreeShallow, genTwoNonOverlappingIdTrees, normalizedBaseCases}

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}

import scala.language.implicitConversions
import scala.math

class IdTreeTest extends ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMaxDiscardRatio(20)

  private val idPord: PartialOrdering[IdTree] = summon[PartialOrdering[IdTree]]

  private val testIds: Seq[IdTree] = Seq(
    seed,
    Branch(1, 1),
    Branch(0, 1),
    Branch(1, 0),
    Branch(1, Branch(1, 1)),
    Branch(1, Branch(1, 0)),
    Branch(Branch(1, 0), 1),
    Branch(Branch(1, 0), 0),
    Branch(Branch(1, 1), Branch(1, 1)),
    Branch(Branch(0, 1), Branch(1, 1)),
    Branch(Branch(1, 0), Branch(0, 1)),
    Branch(Branch(1, 0), Branch(1, 0)),
    Branch(Branch(0, 1), Branch(1, 0)),
    Branch(Branch(0, 1), Branch(0, 1)),
    Branch(Branch(0, 1), Branch(0, 0))
  )

  private val overlappingIdPairs: Seq[(IdTree, IdTree)] = Seq(
    (Leaf(1), Leaf(1)),
    (Branch(1, 1), Leaf(1)),
    (Branch(1, 0), Leaf(1)),
    (Branch(0, 1), Leaf(1)),
    (Branch(Branch(0, 1), 0), Leaf(1)),
    (Branch(Branch(0, 1), 0), Branch(1, 0)),
    (Branch(0, Branch(0, 1)), Branch(1, Branch(0, Branch(0, 1))))
  )

  private val nonOverlappingIdPairs: Seq[(IdTree, IdTree)] = Seq(
    (Leaf(1), Leaf(0)),
    (Branch(1, 1), Leaf(0)),
    (Branch(1, 0), Leaf(0)),
    (Branch(0, 1), Leaf(0)),
    (Branch(Branch(0, 1), 0), Leaf(0)),
    (Branch(Branch(0, 0), 1), Branch(1, 0)),
    (Branch(0, Branch(0, 0)), Branch(1, Branch(0, Branch(0, 1)))),
    (Branch(0, Branch(1, Branch(1, 0))), Branch(1, Branch(0, Branch(0, 1))))
  )

  private val nonNormalizedIds: Seq[IdTree] = Seq(
    Branch(0, 0),
    Branch(1, 1),
    Branch(0, Branch(0, 0)),
    Branch(1, Branch(1, 1)),
    Branch(Branch(0, 0), 0),
    Branch(Branch(1, 1), 1),
    Branch(Branch(1, 1), Branch(1, 1)),
    Branch(Branch(1, 1), Branch(1, Branch(1, 1))),
    Branch(Branch(0, 1), Branch(0, 0))
  )

  test("split works for seed") {
    assertEquals(seed.split, (Branch(1, 0), Branch(0, 1)))
  }

  test("split works on anonymous stamps") {
    assertEquals(anonymous.split, (Leaf(0), Leaf(0)))
  }

  test("split works for left hand of branch") {
    val firstSplit  = seed.split._1
    val secondSplit = firstSplit.split
    assertEquals(secondSplit._1, Branch(Branch(1, 0), 0))
    assertEquals(secondSplit._2, Branch(Branch(0, 1), 0))
  }

  test("split works for right hand of branch") {
    val firstSplit  = seed.split._2
    val secondSplit = firstSplit.split
    assertEquals(secondSplit._1, Branch(0, Branch(1, 0)))
    assertEquals(secondSplit._2, Branch(0, Branch(0, 1)))
  }

  test("split descends into right hand if left hand is 0") {
    val id                        = Branch(0, 1)
    val (firstSplit, secondSplit) = id.split
    assertEquals(firstSplit, Branch(0, Branch(1, 0)))
    assertEquals(secondSplit, Branch(0, Branch(0, 1)))
  }

  test("split descends into left hand if right hand is 0") {
    val id                        = Branch(1, 0)
    val (firstSplit, secondSplit) = id.split
    assertEquals(firstSplit, Branch(Branch(1, 0), 0))
    assertEquals(secondSplit, Branch(Branch(0, 1), 0))
  }

  test("split works for branches that don't have a 0 on either side") {
    val id                        = Branch(1, Branch(1, 0))
    val (firstSplit, secondSplit) = id.split
    assertEquals(firstSplit, Branch(1, 0))
    assertEquals(secondSplit, Branch(0, Branch(1, 0)))
  }

  test("split works if both sides of branch are branches") {
    val branchesWithBranches: Seq[Branch] = Seq(
      Branch(Branch(1, 0), Branch(0, 1)),
      Branch(Branch(1, 0), Branch(1, 0)),
      Branch(Branch(0, 1), Branch(0, 1)),
      Branch(Branch(0, 1), Branch(1, 0))
    )

    branchesWithBranches.foreach { testId =>
      assertEquals(testId.split._1, Branch(testId.left, 0))
      assertEquals(testId.split._2, Branch(0, testId.right))
    }
  }

  property("split produces normalized ids") {
    forAll(genIdTree) { id =>
      val (left, right) = id.split
      assertEquals(left, left.normalized)
      assertEquals(right, right.normalized)
    }
  }

  test("add returns the inverse of split for seed") {
    assertEquals(seed.split._1 + seed.split._2, seed)
  }

  test("add returns 0 for (0 + 0)") {
    assertEquals(anonymous + anonymous, Leaf(0))
  }

  test("add throws an Exception if adding two overlapping ids") {
    intercept[IllegalArgumentException](seed + seed)
    intercept[IllegalArgumentException](Branch(1, 0) + Branch(1, 0))
    intercept[IllegalArgumentException](Branch(0, 1) + Branch(0, 1))
    intercept[IllegalArgumentException](Branch(Branch(0, 0), 1) + Branch(0, 1))
    intercept[IllegalArgumentException](Branch(1, Branch(0, 1)) + Branch(0, 1))
  }

  property("add throws an Exception if adding two generated overlapping ids") {
    forAll(genIdTree, genIdTree) { (left, right) =>
      (left `overlapsWith` right) ==> {
        intercept[IllegalArgumentException](left + right)
        intercept[IllegalArgumentException](right + left): Unit
      }
    }
  }

  test("add returns a normalized id") {
    assertEquals(Branch(1, Branch(1, 0)) + Branch(0, Branch(0, 1)), seed)
    assertEquals(Branch(1, Leaf(1)) + Branch(0, Branch(0, 0)), seed)
    assertEquals(Branch(0, 0) + Leaf(0), Leaf(0))
    assertEquals(Branch(Branch(0, 0), 0) + Leaf(0), Leaf(0))
    assertEquals(Leaf(0) + Branch(Branch(0, 0), 0), Leaf(0))
    assertEquals(Leaf(0) + Branch(Branch(1, 1), 1), Leaf(1))
    assertEquals(Leaf(0) + Branch(Branch(Branch(1, 1), 1), Branch(1, 1)), Leaf(1))
    assertEquals(
      Branch(
        0,
        Branch(1, Branch(0, Branch(1, Branch(0, 1))))
      ) + Branch(Branch(Branch(1, 1), 1), Branch(0, Branch(1, Branch(0, Branch(1, 0))))),
      Leaf(1)
    )
  }

  test("add returns a normalized id for non-overlapping id pair table") {
    nonOverlappingIdPairs.foreach { case (left, right) =>
      assertEquals(left + right, (left + right).normalized)
      assertEquals(right + left, (left + right).normalized)
    }
  }

  property("add returns a normalized id for generated non-overlapping id pairs") {
    forAll(genTwoNonOverlappingIdTrees) { case (left, right) =>
      assertEquals(left + right, (left + right).normalized)
      assertEquals(right + left, (left + right).normalized)
    }
  }

  property("add returns normalized ids for generated pairs") {
    forAll(genTwoNonOverlappingIdTrees) { (left, right) =>
      val sum = left + right
      assertEquals(sum, sum.normalized)
    }
  }

  test("add does not allow adding two disjoint ids") {
    overlappingIdPairs.foreach { case (l, r) =>
      intercept[IllegalArgumentException](l + r)
      intercept[IllegalArgumentException](r + l)
      intercept[IllegalArgumentException](l + l)
      intercept[IllegalArgumentException](r + r)
    }
  }

  test("split is the inverse of add for testIds") {
    testIds.foreach { id =>
      id.split match
          case (l, r) => assertEquals(l + r, id.normalized)
    }
  }

  property("split is the inverse of add for generated ids") {
    forAll(genIdTree) { id =>
      id.split match
          case (l, r) => assertEquals(l + r, id.normalized)
    }
  }

  property("add is commutative") {
    forAll(genIdTree, genIdTree) { (a, b) =>
      !(a `overlapsWith` b) ==> assertEquals(a + b, b + a)
    }
  }

  test("add returns normalizedId reference when adding normalizedId + anonymousEquivalent") {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      assert((normalizedId + Branch(Branch(0, 0), Branch(Branch(0, Branch(0, Branch(0, 0))), 0))) eq normalizedId)
    }
  }

  property("add returns normalizedId reference when adding anonymous + normalizedId") {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      (normalizedId != anonymous) ==> assert((anonymous + normalizedId) eq normalizedId)
    }
  }

  test("NormalForm[IdTree] normalizes to 1 if Tree is all 1s") {
    assertEquals(Leaf(1).normalized, Leaf(1))
    assertEquals(Branch(1, 1).normalized, Leaf(1))
    assertEquals(Branch(Branch(1, 1), 1).normalized, Leaf(1))
    assertEquals(Branch(1, Branch(1, 1)).normalized, Leaf(1))
    assertEquals(Branch(1, Branch(Branch(1, 1), 1)).normalized, Leaf(1))
  }

  test("NormalForm[IdTree] normalizes to 0 if Tree is all 0s") {
    assertEquals(Leaf(0).normalized, Leaf(0))
    assertEquals(Branch(0, 0).normalized, Leaf(0))
    assertEquals(Branch(Branch(0, 0), 0).normalized, Leaf(0))
    assertEquals(Branch(0, Branch(0, 0)).normalized, Leaf(0))
    assertEquals(Branch(0, Branch(Branch(0, 0), 0)).normalized, Leaf(0))
  }

  test("NormalForm[IdTree] normalizes nested id tree correctly") {
    val id = Branch(Branch(1, 1), Branch(Branch(0, Branch(1, Branch(1, 1))), 0))
    assertEquals(id.normalized, Branch(1, Branch(Branch(0, 1), 0)))
  }

  property("NormalForm[IdTree] provides equal ids according to PartialOrdering[IdTree]") {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      (normalizedId != id) ==>
      assertEquals(idPord.tryCompare(normalizedId, id), Some(0))
    }
  }

  test("NormalForm[IdTree] returns this reference if already normalized") {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      assert(normalizedId eq normalizedId.normalized)
    }
  }

  test("overlapsWith is true for overlapping pair table") {
    overlappingIdPairs.foreach { case (left, right) =>
      assertEquals(left `overlapsWith` right, true)
      assertEquals(right `overlapsWith` left, true)
    }
  }

  test("overlapsWith is false for non-overlapping pair table") {
    nonOverlappingIdPairs.foreach { case (left, right) =>
      assertEquals(left `overlapsWith` right, false)
      assertEquals(right `overlapsWith` left, false)
    }
  }

  property("overlapsWith is commutative") {
    forAll(genIdTree, genIdTree) { (left, right) =>
      (left `overlapsWith` right) ==> assertEquals(right `overlapsWith` left, true)
    }
  }

  private def partialOrderProperties(id: IdTree): Unit = {
    val normalizedId = id.normalized
    if normalizedId != seed then {
      assertEquals(idPord.lteq(seed, id), false)
    } else {
      assertEquals(idPord.lteq(seed, id), true)
    }

    assertEquals(idPord.lteq(id, seed), true)

    (normalizedId != anonymous) ==> {
      assertEquals(idPord.lteq(id.split._1, id.split._2), false)
      assertEquals(idPord.lteq(id.split._2, id.split._1), false)

      assertEquals(idPord.lteq(id.split._1, id), true)
      assertEquals(idPord.lteq(id, id.split._1), false)

      assertEquals(idPord.lteq(id.split._2, id), true)
      assertEquals(idPord.lteq(id, id.split._2), false)

      assertEquals(idPord.lteq(id.split._1.split._1, id), true)
      assertEquals(idPord.lteq(id, id.split._1.split._1), false)
      assertEquals(idPord.lteq(id.split._1.split._2, id), true)
    }
  }

  test("PartialOrdering[IdTree].lteq works for testIds") {
    testIds.foreach(partialOrderProperties)
  }

  test("PartialOrdering[IdTree].lteq works for non-normalized test ids") {
    nonNormalizedIds.foreach(partialOrderProperties)
  }

  test("PartialOrdering[IdTree].lteq works for anonymous id") {
    assertEquals(idPord.lteq(anonymous, anonymous), true)
    assertEquals(idPord.lteq(anonymous, seed), true)
    assertEquals(idPord.lteq(seed, anonymous), false)
    assertEquals(idPord.lteq(anonymous, Branch(0, 0)), true)
    assertEquals(idPord.lteq(anonymous, Branch(1, 0)), true)

    forAll(genIdTree) { id =>
      assertEquals(idPord.lteq(anonymous, id), true)
    }
  }

  test("PartialOrdering[IdTree].lteq works for normalized generated ids") {
    forAll(genIdTreeBySplitting)(partialOrderProperties)
  }

  test("PartialOrdering[IdTree].lteq works for ids generated using splitting") {
    forAll(genIdTreeBySplitting)(partialOrderProperties)
  }

  test("PartialOrdering[IdTree].lteq works for non-normalized generated ids") {
    forAll(genIdTree)(partialOrderProperties)
  }

  test("PartialOrdering[IdTree].lteq works for incomparable ids") {
    assertEquals(idPord.lteq(Branch(Branch(0, 1), 1), Branch(1, 0)), false)
    assertEquals(idPord.lteq(Branch(1, 0), Branch(Branch(0, 1), 1)), false)
  }

  property("PartialOrdering[IdTree].tryCompare is Some(0) if a <= b && b <= a") {
    forAll(genIdTreeShallow, genIdTreeShallow) { (idA: IdTree, idB: IdTree) =>
      (idPord.lteq(idA, idB) && idPord.lteq(idB, idA)) ==> {
        assertEquals(idPord.tryCompare(idA, idB), Some(0))
        assertEquals(idPord.tryCompare(idB, idA), Some(0))
      }
    }
  }

  property("PartialOrdering[IdTree].tryCompare is Some(-1) / Some(1) if a <= b && !(b <= a) / b <= a && !(a <= b)") {
    forAll(genIdTree, genIdTree) { (idA: IdTree, idB: IdTree) =>
      (idPord.lteq(idA, idB) != idPord.lteq(idB, idA)) ==> {
        if idPord.lteq(idA, idB) then {
          assertEquals(idPord.tryCompare(idA, idB), Some(-1))
          assertEquals(idPord.tryCompare(idB, idA), Some(1))
        } else {
          assertEquals(idPord.tryCompare(idA, idB), Some(1))
          assertEquals(idPord.tryCompare(idB, idA), Some(-1))
        }
      }
    }
  }

  property("PartialOrdering[IdTree].tryCompare is None if not comparable") {
    forAll(genIdTree, genIdTree) { (idA: IdTree, idB: IdTree) =>
      (!idPord.lteq(idA, idB) && !idPord.lteq(idB, idA)) ==> {
        assertEquals(idPord.tryCompare(idA, idB), None)
        assertEquals(idPord.tryCompare(idB, idA), None)
      }
    }
  }
}
