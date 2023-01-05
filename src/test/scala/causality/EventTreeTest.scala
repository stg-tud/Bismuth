package de.tu_darmstadt.stg.daimpl

package causality

import causality.EventTree.{Branch, Leaf, seed, given}
import causality.EventTreeGenerators.{genEventTree, genEventTreeLeaf, genRandomEventTree}
import causality.IdTreeGenerators.genIdTree

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.annotation.tailrec
import scala.language.implicitConversions

class EventTreeTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val eventTreePord = EventTree.partialOrdering

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(maxDiscardedFactor = 20.0)

  final def isNormalized(eventTree: EventTree): Boolean = eventTree match {
    case Leaf(_)                         => true
    case Branch(_, Leaf(0), r)           => isNormalized(r)
    case Branch(_, Branch(0, ll, lr), r) => isNormalized(ll) && isNormalized(lr) && isNormalized(r)
    case Branch(_, l, Leaf(0))           => isNormalized(l)
    case Branch(_, l, Branch(0, rl, rr)) => isNormalized(l) && isNormalized(rl) && isNormalized(rr)
    case _                               => false
  }

  "Leaf" should "only accept positive values" in {
    assertThrows[IllegalArgumentException](Leaf(-1))
    assertThrows[IllegalArgumentException](Leaf(-401))
  }

  "Branch" should "only accept positive values" in {
    assertThrows[IllegalArgumentException](Branch(-1, 23, 4))
    assertThrows[IllegalArgumentException](Branch(-1234, 0, 0))
  }

  "min" should "work for Leaf(n)" in {
    assert(seed.min == 0)
    assert(Leaf(1).min == 1)
    assert(Leaf(42).min == 42)
  }

  it should "work for branch" in {
    assert(Branch(2, 1, 2).min == 3)
    assert(Branch(2, Branch(1, 2, 4), 2).min == 4)
  }

  "max" should "work for Leaf(n)" in {
    assert(seed.max == 0)
    assert(Leaf(1).max == 1)
    assert(Leaf(42).max == 42)
  }

  it should "work for branch" in {
    assert(Branch(2, 1, 2).max == 4)
    assert(Branch(2, Branch(1, 2, 4), 2).max == 7)
  }

  "lift" should "work for Leaf" in {
    assert(Leaf(1).lift(1) == Leaf(2))
    assert(Leaf(2).lift(5) == Leaf(7))
  }

  it should "work for simple Branch" in {
    assert(Branch(1, 2, 2).lift(1) == Branch(2, 2, 2))
    assert(Branch(7, 0, 3).lift(4) == Branch(11, 0, 3))
  }

  it should "work for nested Branch" in {
    assert(Branch(1, Branch(2, 0, 4), 2).lift(1) == Branch(2, Branch(2, 0, 4), 2))
    assert(Branch(7, 0, Branch(2, 0, 4)).lift(4) == Branch(11, 0, Branch(2, 0, 4)))
  }

  "sink" should "work for Leaf" in {
    assert(Leaf(1).sink(1) == Leaf(0))
    assert(Leaf(5).sink(2) == Leaf(3))
  }

  it should "work for Branch" in {
    assert(Branch(2, 4, 1).sink(2) == Branch(0, 4, 1))
    assert(Branch(2, 5, Branch(1, 2, 3)).sink(2) == Branch(0, 5, Branch(1, 2, 3)))
  }

  it should "refuse sinking beyond 0" in {
    assertThrows[IllegalArgumentException](Leaf(1).sink(2))
  }

  "join" should "work for two Leafs" in {
    (Leaf(1) join Leaf(1)) shouldBe Leaf(1)
    (Leaf(42) join Leaf(0)) shouldBe Leaf(42)
    (Leaf(0) join Leaf(1)) shouldBe Leaf(1)
  }

  it should "work for Leaf and Branch" in {
    (Leaf(42) join Branch(2, 4, 5)) shouldBe Leaf(42)
    (Branch(2, 4, 5) join Leaf(42)) shouldBe Leaf(42)
  }

  it should "produce normalized Ids" in {
    forAll(genEventTree, genEventTreeLeaf) { (ev1, ev2) =>
      val joined = ev1 join ev2
      joined shouldBe joined.normalized
    }
  }

  it should "be commutative" in {
    forAll(genEventTree, genEventTree) { (ev1, ev2) =>
      (ev1 join ev2) shouldBe (ev2 join ev1)
    }
  }

  "fill" should "return same reference if not fillable" in {
    val eventsAndIds = Table(
      ("eventTree", "idTree"),
      (Leaf(1), IdTree.Leaf(0)),
      (Leaf(0), IdTree.Leaf(1)),
      (Leaf(42), IdTree.Leaf(1)),
      (Branch(0, 1, 0), IdTree.Branch(0, IdTree.Branch(1, 0))),
      (Branch(1, 3, 0), IdTree.Leaf(0)),
      (Branch(1, 0, 3), IdTree.Leaf(0)),
      (Branch(1, 3, 0), IdTree.Branch(1, 0)),
      (Branch(1, 0, 3), IdTree.Branch(0, 1)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(0, 1)),
      (Branch(1, 3, Branch(0, 3, 0)), IdTree.Branch(1, 0)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(IdTree.Branch(1, 0), 0)),
      (Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)), IdTree.Branch(0, IdTree.Branch(1, 0))),
      (Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)), IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(1, 0))),
    )

    forAll(eventsAndIds) { (ev, id) =>
      assert(ev eq ev.fill(id))
    }
  }

  it should "simplify the EventTree if possible" in {
    val fillTestTable = Table(
      ("eventTree", "idTree", "expectedFilledEventTree"),
      (Branch(1, 3, 0), IdTree.Leaf(1), Leaf(4)),
      (Branch(1, 0, 3), IdTree.Leaf(1), Leaf(4)),
      (Branch(1, Branch(4, 0, 0), 4), IdTree.Leaf(1), Leaf(5)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Leaf(1), Leaf(4)),
      (Branch(1, 4, Branch(3, 1, 0)), IdTree.Leaf(1), Leaf(5)),
      (Branch(0, Branch(0, Branch(0, 0, 1), 2), Branch(0, 2, 0)), IdTree.Leaf(1), Leaf(2)),
      (Branch(0, Branch(0, Branch(1, 0, 0), 2), Branch(0, 0, 2)), IdTree.Leaf(1), Leaf(2)),
      (Branch(1, 0, 3), IdTree.Branch(1, 0), Leaf(4)),
      (Branch(1, 3, 0), IdTree.Branch(0, 1), Leaf(4)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(IdTree.Branch(0, 1), 0), Leaf(4)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(IdTree.Branch(0, 1), 1), Leaf(4)),
      (
        Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)),
        IdTree.Branch(0, IdTree.Branch(0, 1)),
        Branch(0, Branch(0, 0, 2), 2)
      ),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(1, 0), Leaf(4)),
      (Branch(1, 3, Branch(0, 2, 0)), IdTree.Branch(0, 1), Leaf(4)),
      (Branch(1, Branch(0, 0, 2), Branch(0, 2, 0)), IdTree.Branch(0, 1), Branch(1, Branch(0, 0, 2), 2)),
      (
        Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)),
        IdTree.Branch(IdTree.Branch(1, 0), IdTree.Branch(1, 0)),
        Branch(0, 2, Branch(0, 2, 0))
      ),
      (Branch(0, Branch(0, Branch(0, 0, 1), 2), Branch(0, 2, 0)), IdTree.Branch(IdTree.Branch(1, 0), 1), Leaf(2)),
      (Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)), IdTree.Branch(IdTree.Branch(1, 0), IdTree.Branch(0, 1)), Leaf(2)),
      (
        Branch(0, Branch(0, Branch(0, 0, 1), 1), Branch(0, 2, 0)),
        IdTree.Branch(IdTree.Branch(1, 0), 1),
        Branch(1, 0, 1)
      ),
      (
        Branch(0, Branch(0, Branch(0, 0, 1), 2), Branch(0, 2, 0)),
        IdTree.Branch(1, IdTree.Branch(1, 0)),
        Branch(0, 2, Branch(0, 2, 0))
      )
    )

    forAll(fillTestTable) { (ev, id, expectedEvFilled) =>
      val evFilled = ev.fill(id)
      evFilled shouldBe expectedEvFilled
      eventTreePord.tryCompare(evFilled, ev) shouldBe Some(1)
      eventTreePord.tryCompare(ev, evFilled) shouldBe Some(-1)
    }
  }

  it should "return same reference if equal to EventTree for generated EventTree and Id" in {
    forAll(genEventTree.map(_.normalized), genIdTree.suchThat(!_.isAnonymous).map(_.normalized)) { (ev, id) =>
      val evFilled = ev.fill(id)
      whenever(evFilled == ev) {
        assert(evFilled eq ev)
      }
    }
  }

  it should "be greater according to PartialOrdering after successful fill" in {
    forAll(genEventTree.map(_.normalized), genIdTree.suchThat(!_.isAnonymous).map(_.normalized)) { (ev, id) =>
      val evFilled = ev.fill(id)
      whenever(!(evFilled eq ev)) {
        isNormalized(evFilled) shouldBe true // Assumption of PartialOrdering
        eventTreePord.tryCompare(evFilled, ev) shouldBe Some(1)
        eventTreePord.tryCompare(ev, evFilled) shouldBe Some(-1)
      }
    }
  }

  "increment" should "work" in {
    ???
  }

  "PartialOrderingEventTree.lteq" should "work" in {
    ???
  }

  "PartialOrderingEventTree.tryCompare" should "work" in {
    ???
  }

  it should "be Some(0) when a == b" in {
    forAll(genEventTree) { ev =>
      eventTreePord.tryCompare(ev, ev) shouldBe Some(0)
    }
  }

  it should "be Some(1) or Some(-1) when a <= b xor b <= a" in {
    forAll(genEventTree, genEventTree) { (left, right) =>
      whenever(left <= right ^ right <= left) {
        if (left <= right) {
          eventTreePord.tryCompare(left, right) shouldBe Some(-1)
          eventTreePord.tryCompare(right, left) shouldBe Some(1)
        } else {
          eventTreePord.tryCompare(left, right) shouldBe Some(1)
          eventTreePord.tryCompare(right, left) shouldBe Some(-1)
        }
      }
    }
  }

  "NormalForm[EventTree]" should "work for Leaves" in {
    forAll(Gen.posNum[Int]) { (n: Int) =>
      Leaf(n).normalized shouldBe Leaf(n)
    }
  }

  it should "work for Branches" in {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int]) { (value: Int, leftLeafValue: Int, rightLeafValue: Int) =>
      val minLeaf = Math.min(leftLeafValue, rightLeafValue)
      val normalizedEventTree = Branch(
        value,
        Leaf(leftLeafValue),
        Leaf(rightLeafValue)
      ).normalized
      if (leftLeafValue == rightLeafValue) {
        normalizedEventTree shouldBe Leaf(value + leftLeafValue)
      } else {
        normalizedEventTree shouldBe Branch(
          value + minLeaf,
          leftLeafValue - minLeaf,
          rightLeafValue - minLeaf
        )
      }
    }
  }

  it should "work for nested examples" in {
    val nestedEventTrees = Table(
      ("event", "expectedNormalForm"),
      Branch(0, Branch(0, 0, 0), Leaf(0))           -> Leaf(0),
      Branch(2, Branch(8, 9, 10), Branch(3, 3, 10)) -> Branch(8, Branch(11, 0, 1), Branch(0, 0, 7)),
      Branch(2, Branch(8, 0, 10), Branch(3, 3, 10)) -> Branch(8, Branch(2, 0, 10), Branch(0, 0, 7)),
      Branch(2, Leaf(3), Branch(3, 3, 10))          -> Branch(5, Leaf(0), Branch(3, 0, 7)),
      Branch(3, Branch(2, Branch(2, 2, 2), Leaf(4)), Branch(0, Leaf(6), Branch(0, 6, 6))) -> Leaf(9)
    )

    forAll(nestedEventTrees) { (id, expectedNormalForm) =>
      id.normalized shouldBe expectedNormalForm
    }
  }

  it should "produce event trees that are normalized" in {
    forAll(genEventTree) { ev =>
      isNormalized(ev.normalized)
    }

    forAll(genRandomEventTree) { ev =>
      isNormalized(ev.normalized)
    }
  }

  it should "return `this` reference if already normalized" in {
    forAll(genEventTree) { evTree =>
      val normalizedEv = evTree.normalized
      assert(normalizedEv eq normalizedEv.normalized)
    }

    forAll(genRandomEventTree) { evTree =>
      val normalizedEv = evTree.normalized
      assert(normalizedEv eq normalizedEv.normalized)
    }
  }
}
