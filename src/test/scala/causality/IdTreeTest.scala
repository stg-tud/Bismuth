package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf, anonymous, seed, given}
import causality.IdTreeGenerators.{genIdTree, genIdTreeBySplitting, genIdTreeShallow, genTwoNonOverlappingIdTrees, normalizedBaseCases}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.{TableFor1, TableFor2}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions
import scala.math

class IdTreeTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(maxDiscardedFactor = 20.0)

  val idPord: PartialOrdering[IdTree] = summon[PartialOrdering[IdTree]]

  val testIds: TableFor1[IdTree] = Table(
    "id",
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

  val overlappingIdPairTable: TableFor2[IdTree, IdTree] = Table(
    ("left", "right"),
    (Leaf(1), Leaf(1)),
    (Branch(1, 1), Leaf(1)),
    (Branch(1, 0), Leaf(1)),
    (Branch(0, 1), Leaf(1)),
    (Branch(Branch(0, 1), 0), Leaf(1)),
    (Branch(Branch(0, 1), 0), Branch(1, 0)),
    (Branch(0, Branch(0, 1)), Branch(1, Branch(0, Branch(0, 1))))
  )

  val nonOverlappingIdPairs: TableFor2[IdTree, IdTree] = Table(
    ("left", "right"),
    (Leaf(1), Leaf(0)),
    (Branch(1, 1), Leaf(0)),
    (Branch(1, 0), Leaf(0)),
    (Branch(0, 1), Leaf(0)),
    (Branch(Branch(0, 1), 0), Leaf(0)),
    (Branch(Branch(0, 0), 1), Branch(1, 0)),
    (Branch(0, Branch(0, 0)), Branch(1, Branch(0, Branch(0, 1)))),
    (Branch(0, Branch(1, Branch(1, 0))), Branch(1, Branch(0, Branch(0, 1))))
  )

  val nonNormalizedIds: TableFor1[IdTree] = Table(
    "id",
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

  "Leaf" should "only accept 0 and 1 as value" in {
    assertDoesNotCompile("Leaf(-1)")
    assertDoesNotCompile("Leaf(2)")
  }

  "split" should "work for seed" in {
    assert(seed.split == (Branch(1, 0), Branch(0, 1)))
  }

  it should "work on anonymous stamps" in {
    assert(anonymous.split == (Leaf(0), Leaf(0)))
  }

  it should "work for left hand of branch" in {
    val firstSplit  = seed.split._1
    val secondSplit = firstSplit.split
    assert(secondSplit._1 == Branch(Branch(1, 0), 0))
    assert(secondSplit._2 == Branch(Branch(0, 1), 0))
  }

  it should "work for right hand of branch" in {
    val firstSplit  = seed.split._2
    val secondSplit = firstSplit.split
    assert(secondSplit._1 == Branch(0, Branch(1, 0)))
    assert(secondSplit._2 == Branch(0, Branch(0, 1)))
  }

  it should "descend into right hand if left hand is 0" in {
    val id                        = Branch(0, 1)
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(0, Branch(1, 0)))
    assert(secondSplit == Branch(0, Branch(0, 1)))
  }

  it should "descend into left hand if right hand is 0" in {
    val id                        = Branch(1, 0)
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(Branch(1, 0), 0))
    assert(secondSplit == Branch(Branch(0, 1), 0))
  }

  it should "work for branches that don't have a 0 on either side" in {
    val id                        = Branch(1, Branch(1, 0))
    val (firstSplit, secondSplit) = id.split
    assert(firstSplit == Branch(1, 0))
    assert(secondSplit == Branch(0, Branch(1, 0)))
  }

  it should "work if both sides of branch are branches" in {
    val branchesWithBranches = Table[Branch](
      "id",
      Branch(Branch(1, 0), Branch(0, 1)),
      Branch(Branch(1, 0), Branch(1, 0)),
      Branch(Branch(0, 1), Branch(0, 1)),
      Branch(Branch(0, 1), Branch(1, 0))
    )

    forAll(branchesWithBranches) { testId =>
      testId.split._1 shouldBe Branch(testId.left, 0)
      testId.split._2 shouldBe Branch(0, testId.right)
    }
  }

  it should "produce normalized ids" in {
    forAll(genIdTree) { id =>
      val (left, right) = id.split
      left shouldBe left.normalized
      right shouldBe right.normalized
    }
  }

  "add" should "return the inverse of split for seed" in {
    (seed.split._1 + seed.split._2) shouldBe seed
  }

  it should "return 0 for (0 + 0)" in {
    (anonymous + anonymous) shouldBe Leaf(0)
  }

  it should "throw an Exception if adding two overlapping ids" in {
    assertThrows[IllegalArgumentException](seed + seed)
    assertThrows[IllegalArgumentException](Branch(1, 0) + Branch(1, 0))
    assertThrows[IllegalArgumentException](Branch(0, 1) + Branch(0, 1))
    assertThrows[IllegalArgumentException](Branch(Branch(0, 0), 1) + Branch(0, 1))
    assertThrows[IllegalArgumentException](Branch(1, Branch(0, 1)) + Branch(0, 1))
  }

  it should "throw an Exception if adding two generated overlapping ids" in {
    forAll(genIdTree, genIdTree) { (left, right) =>
      whenever(left overlapsWith right) {
        assertThrows[IllegalArgumentException](left + right)
        assertThrows[IllegalArgumentException](right + left)
      }
    }
  }

  it should "return a normalized id" in {
    (Branch(1, Branch(1, 0)) + Branch(0, Branch(0, 1))) shouldBe seed
    (Branch(1, Leaf(1)) + Branch(0, Branch(0, 0))) shouldBe seed
    (Branch(0, 0) + Leaf(0)) shouldBe Leaf(0)
    (Branch(Branch(0, 0), 0) + Leaf(0)) shouldBe Leaf(0)
    (Leaf(0) + Branch(Branch(0, 0), 0)) shouldBe Leaf(0)
    (Leaf(0) + Branch(Branch(1, 1), 1)) shouldBe Leaf(1)
    (Leaf(0) + Branch(Branch(Branch(1, 1), 1), Branch(1, 1))) shouldBe Leaf(1)
    (Branch(0, Branch(1, Branch(0, Branch(1, Branch(0, 1))))) +
      Branch(Branch(Branch(1, 1), 1), Branch(0, Branch(1, Branch(0, Branch(1, 0)))))) shouldBe Leaf(1)
  }

  it should "return a normalized id for non-overlapping id pair table" in {
    forAll(nonOverlappingIdPairs) { case (left, right) =>
      (left + right) shouldBe (left + right).normalized
      (right + left) shouldBe (left + right).normalized
    }
  }

  it should "return a normalized id for generated non-overlapping id pairs" in {
    forAll(genTwoNonOverlappingIdTrees) { case (left, right) =>
      (left + right) shouldBe (left + right).normalized
      (right + left) shouldBe (left + right).normalized
    }
  }

  it should "return normalized ids for generated pairs" in {
    forAll(genTwoNonOverlappingIdTrees) { (left, right) =>
      val sum = left + right
      sum shouldBe sum.normalized
    }
  }

  it should "not allow adding two disjoint ids" in {
    forAll(overlappingIdPairTable) { (l, r) =>
      assertThrows[IllegalArgumentException](l + r)
      assertThrows[IllegalArgumentException](r + l)
      assertThrows[IllegalArgumentException](l + l)
      assertThrows[IllegalArgumentException](r + r)
    }
  }

  it should "be the inverse of split for testIds" in {
    forAll(testIds) { id =>
      id.split match
        case (l, r) => (l + r) shouldBe id.normalized
    }
  }

  it should "be the inverse of split for generated ids" in {
    forAll(genIdTree) { id =>
      id.split match
        case (l, r) => (l + r) shouldBe id.normalized
    }
  }

  it should "be commutative" in {
    forAll(genIdTree, genIdTree) { (a, b) =>
      whenever(!(a overlapsWith b)) {
        a + b shouldBe b + a
      }
    }
  }

  it should "return `normalizedId` reference when adding `normalizedId` + `anonymousEquivalent`" in {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      assert(
        (normalizedId + Branch(Branch(0, 0), Branch(Branch(0, Branch(0, Branch(0, 0))), 0))) eq normalizedId
      )
    }
  }

  it should "return `normalizedId` reference when adding `anonymous` + `normalizedId`" in {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      whenever(normalizedId != anonymous) {
        assert((anonymous + normalizedId) eq normalizedId)
      }
    }
  }

  "NormalForm[IdTree]" should "normalize to 1 if Tree is all 1s" in {
    assert(Leaf(1).normalized == Leaf(1))
    assert(Branch(1, 1).normalized == Leaf(1))
    assert(Branch(Branch(1, 1), 1).normalized == Leaf(1))
    assert(Branch(1, Branch(1, 1)).normalized == Leaf(1))
    assert(Branch(1, Branch(Branch(1, 1), 1)).normalized == Leaf(1))
  }

  it should "normalize to 0 if Tree is all 0s" in {
    assert(Leaf(0).normalized == Leaf(0))
    assert(Branch(0, 0).normalized == Leaf(0))
    assert(Branch(Branch(0, 0), 0).normalized == Leaf(0))
    assert(Branch(0, Branch(0, 0)).normalized == Leaf(0))
    assert(Branch(0, Branch(Branch(0, 0), 0)).normalized == Leaf(0))
  }

  it should "normalize nested id tree correctly" in {
    val id = Branch(Branch(1, 1), Branch(Branch(0, Branch(1, Branch(1, 1))), 0))
    assert(id.normalized == Branch(1, Branch(Branch(0, 1), 0)))
  }

  it should "provide equal ids according to PartialOrdering[IdTree]" in {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      whenever(normalizedId != id) {
        idPord.tryCompare(normalizedId, id) shouldBe Some(0)
      }
    }
  }

  it should "return `this` reference if already normalized" in {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalized
      assert(normalizedId eq normalizedId.normalized)
    }
  }

  "overlapsWith" should "be true for overlapping pair table" in {
    forAll(overlappingIdPairTable) { (left, right) =>
      left overlapsWith right shouldBe true
      right overlapsWith left shouldBe true
    }
  }

  it should "be false for non-overlapping pair table" in {
    forAll(nonOverlappingIdPairs) { case (left, right) =>
      left overlapsWith right shouldBe false
      right overlapsWith left shouldBe false
    }
  }

  it should "be commutative" in {
    forAll(genIdTree, genIdTree) { (left, right) =>
      whenever(left overlapsWith right) {
        right overlapsWith left shouldBe true
      }
    }
  }

  def partialOrderProperties(id: IdTree): Unit = {
    val normalizedId = id.normalized
    if (normalizedId != seed) {
      idPord.lteq(seed, id) shouldBe false
    } else {
      idPord.lteq(seed, id) shouldBe true
    }

    idPord.lteq(id, seed) shouldBe true

    whenever(normalizedId != anonymous) {
      idPord.lteq(id.split._1, id.split._2) shouldBe false
      idPord.lteq(id.split._2, id.split._1) shouldBe false

      idPord.lteq(id.split._1, id) shouldBe true
      idPord.lteq(id, id.split._1) shouldBe false

      idPord.lteq(id.split._2, id) shouldBe true
      idPord.lteq(id, id.split._2) shouldBe false

      idPord.lteq(id.split._1.split._1, id) shouldBe true
      idPord.lteq(id, id.split._1.split._1) shouldBe false
      idPord.lteq(id.split._1.split._2, id) shouldBe true
    }
  }

  "PartialOrdering[IdTree].lteq" should "work for testIds" in {
    forAll(testIds)(partialOrderProperties)
  }

  it should "work for non-normalized test ids" in {
    forAll(nonNormalizedIds)(partialOrderProperties)
  }

  it should "work for anonymous id" in {
    idPord.lteq(anonymous, anonymous) shouldBe true
    idPord.lteq(anonymous, seed) shouldBe true
    idPord.lteq(seed, anonymous) shouldBe false
    idPord.lteq(anonymous, Branch(0, 0)) shouldBe true
    idPord.lteq(anonymous, Branch(1, 0)) shouldBe true

    forAll(genIdTree) { id =>
      idPord.lteq(anonymous, id) shouldBe true
    }
  }

  it should "work for normalized generated ids" in {
    forAll(genIdTreeBySplitting)(partialOrderProperties)
  }

  it should "work for ids generated using splitting" in {
    forAll(genIdTreeBySplitting)(partialOrderProperties)
  }

  it should "work for non-normalized generated ids" in {
    forAll(genIdTree)(partialOrderProperties)
  }

  it should "work for incomparable ids" in {
    idPord.lteq(Branch(Branch(0, 1), 1), Branch(1, 0)) shouldBe false
    idPord.lteq(Branch(1, 0), Branch(Branch(0, 1), 1)) shouldBe false
  }

  "PartialOrdering[IdTree].tryCompare" should "be Some(0) if a <= b && b <= a" in {
    forAll(genIdTreeShallow, genIdTreeShallow) { (idA: IdTree, idB: IdTree) =>
      whenever(idPord.lteq(idA, idB) && idPord.lteq(idB, idA)) {
        idPord.tryCompare(idA, idB) shouldBe Some(0)
        idPord.tryCompare(idB, idA) shouldBe Some(0)
      }
    }
  }

  it should "be Some(-1) / Some(1) if a <= b && !(b <= a) / b <= a && !(a <= b)" in {
    forAll(genIdTree, genIdTree) { (idA: IdTree, idB: IdTree) =>
      whenever(idPord.lteq(idA, idB) != idPord.lteq(idB, idA)) {
        if (idPord.lteq(idA, idB)) {
          idPord.tryCompare(idA, idB) shouldBe Some(-1)
          idPord.tryCompare(idB, idA) shouldBe Some(1)
        } else {
          idPord.tryCompare(idA, idB) shouldBe Some(1)
          idPord.tryCompare(idB, idA) shouldBe Some(-1)
        }
      }
    }
  }

  it should "be None, if not comparable" in {
    forAll(genIdTree, genIdTree) { (idA: IdTree, idB: IdTree) =>
      whenever(!idPord.lteq(idA, idB) && !idPord.lteq(idB, idA)) {
        idPord.tryCompare(idA, idB) shouldBe None
        idPord.tryCompare(idB, idA) shouldBe None
      }
    }
  }
}
