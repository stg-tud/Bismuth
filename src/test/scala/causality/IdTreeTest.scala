package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf, anonymous, seed, given}
import causality.IdTreeGenerators.{genIdTree, normalizedBaseCases}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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

  val overlappingIdPairs: TableFor2[IdTree, IdTree] = Table(
    ("left", "right"),
    (Leaf(1), Leaf(1)),
    (Branch(1, 1), Leaf(1)),
    (Branch(1, 0), Leaf(1)),
    (Branch(0, 1), Leaf(1)),
    (Branch(Branch(0, 1), 0), Leaf(1)),
    (Branch(Branch(0, 1), 0), Branch(1, 0)),
    (Branch(0, Branch(0, 1)), Branch(1, Branch(0, Branch(0, 1))))
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
    val branches = Table[Branch](
      "id",
      Branch(Branch(1, 0), Branch(0, 1)),
      Branch(Branch(1, 0), Branch(1, 0)),
      Branch(Branch(0, 1), Branch(0, 1)),
      Branch(Branch(0, 1), Branch(1, 0))
    )

    forAll(branches) { testId =>
      testId.split._1 shouldBe Branch(testId.left, 0)
      testId.split._2 shouldBe Branch(0, testId.right)
    }
  }

  "add" should "return the inverse of split for seed" in {
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

  it should "not allow adding two disjoint ids" in {
    forAll(overlappingIdPairs) { (l, r) =>
      (l + r).isEmpty shouldBe true
      (r + l).isEmpty shouldBe true
      (l + l).isEmpty shouldBe true
      (r + r).isEmpty shouldBe true
    }
  }

  it should "be the inverse of split for testIds" in {
    forAll(testIds) { id =>
      id.split match
        case (l, r) => (l + r).get shouldBe id.normalize
    }
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

  it should "provide equal ids according to PartialOrdering[IdTree]" in {
    forAll(genIdTree) { id =>
      val normalizedId = id.normalize
      whenever(normalizedId != id) {
        idPord.tryCompare(normalizedId, id) shouldBe Some(0)
      }
    }
  }

  def partialOrderProperties(id: IdTree): Unit = {
    val normalizedId = id.normalize
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
    forAll(genIdTree)(partialOrderProperties)
  }

  it should "work for non-normalized generated ids" in {
    forAll(genIdTree)(partialOrderProperties)
  }

  it should "work for incomparable ids" in {
    idPord.lteq(Branch(Branch(0, 1), 1), Branch(1, 0)) shouldBe false
    idPord.lteq(Branch(1, 0), Branch(Branch(0, 1), 1)) shouldBe false
  }

  "PartialOrdering[IdTree].tryCompare" should "be Some(0) if a <= b && b <= a" in {
    forAll(genIdTree, genIdTree) { (idA: IdTree, idB: IdTree) =>
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
