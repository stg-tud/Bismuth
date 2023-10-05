package com.github.ckuessner
package causality

import causality.EventTree.{Branch, Leaf, seed, given}
import causality.EventTreeGenerators.{genEventTree, genEventTreeLeaf, genRandomEventTree}
import causality.IdTreeGenerators.genIdTree

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.prop.{TableFor2, TableFor3}
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
      (Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)), IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(1, 0)))
    )

    forAll(eventsAndIds) { (ev, id) =>
      assert(ev eq ev.fill(id))
    }
  }

  val fillTestTable: TableFor3[Branch, IdTree, EventTree] = Table(
    ("eventTree", "idTree", "expectedFilledEventTree"),
    (Branch(1, 3, 0), IdTree.Leaf(1), Leaf(4)),
    (Branch(1, 0, 3), IdTree.Leaf(1), Leaf(4)),
    (Branch(1, Branch(0, 0, 4), 4), IdTree.Leaf(1), Leaf(5)),
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

  it should "simplify the EventTree if possible" in {
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

  "grow" should "work for non-fillable examples and IdTree with single 1-Leaf" in {
    val eventsAndIds = Table(
      ("eventTree", "idTree", "expectedGrownEventTree"),
      (Leaf(0), IdTree.Leaf(1), Leaf(1)),
      (Leaf(42), IdTree.Leaf(1), Leaf(43)),
      (Branch(1, 3, 0), IdTree.Branch(1, 0), Branch(1, 4, 0)),
      (Branch(1, 0, 3), IdTree.Branch(0, 1), Branch(1, 0, 4)),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(0, 1), Branch(1, Branch(0, 3, 0), 4)),
      (Branch(1, 3, Branch(0, 3, 0)), IdTree.Branch(1, 0), Branch(1, 4, Branch(0, 3, 0))),
      (Branch(1, Branch(0, 3, 0), 3), IdTree.Branch(IdTree.Branch(1, 0), 0), Branch(1, Branch(0, 4, 0), 3)),
      (
        Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)),
        IdTree.Branch(0, IdTree.Branch(1, 0)),
        Branch(0, Branch(0, 0, 2), Branch(0, 3, 0))
      )
    )

    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.grow(id)._1 shouldBe expectedEv
      ev.increment(id) shouldBe expectedEv
    }
  }

  it should "work when needing to split a Leaf" in {
    val eventsAndIds = Table(
      ("eventTree", "idTree", "expectedGrownEventTree"),
      (Leaf(0), IdTree.Branch(0, 1), Branch(0, 0, 1)),
      (Leaf(0), IdTree.Branch(1, 0), Branch(0, 1, 0)),
      (Leaf(1), IdTree.Branch(0, 1), Branch(1, 0, 1)),
      (Leaf(42), IdTree.Branch(1, 0), Branch(42, 1, 0)),
      (Branch(42, 0, 1), IdTree.Branch(0, IdTree.Branch(0, 1)), Branch(42, 0, Branch(1, 0, 1))),
      (Branch(42, 99, 0), IdTree.Branch(IdTree.Branch(1, 0), 0), Branch(42, Branch(99, 1, 0), 0))
    )

    // Test if test inputs are not fillable
    forAll(eventsAndIds) { (ev, id, _) =>
      ev.fill(id) shouldBe ev
    }

    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.grow(id)._1 shouldBe expectedEv
    }

    // Sanity check
    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.increment(id) shouldBe expectedEv
    }
  }

  it should "prefer increment closest to root for IdTree with multiple 1-leafs" in {
    val eventsAndIds = Table(
      ("eventTree", "idTree", "expectedGrownEventTree"),
      // Two 1-leafs
      (
        Branch(0, 0, Branch(0, 0, 1)),
        IdTree.Branch(1, IdTree.Branch(0, 1)),
        Branch(0, 1, Branch(0, 0, 1))
      ),
      (
        Branch(0, Branch(0, 0, 1), 1),
        IdTree.Branch(IdTree.Branch(0, 1), 1),
        Branch(0, Branch(0, 0, 1), 2)
      ),
      (
        Branch(0, Branch(0, 0, 1), Branch(0, 0, Branch(0, 0, 1))),
        IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(0, IdTree.Branch(0, 1))),
        Branch(0, Branch(0, 0, 2), Branch(0, 0, Branch(0, 0, 1)))
      ),
      // Two equally distant increments -> Prefer right (as in paper)
      (
        Branch(0, Branch(0, 0, 2), Branch(0, 2, 0)),
        IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(1, 0)),
        Branch(0, Branch(0, 0, 2), Branch(0, 3, 0))
      ),
      // Three 1-leafs
      (
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, Branch(0, 1, 0), Branch(0, 0, 1))),
        IdTree.Branch(IdTree.Branch(0, IdTree.Branch(0, 1)), IdTree.Branch(IdTree.Branch(1, 0), IdTree.Branch(0, 1))),
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, Branch(0, 1, 0), Branch(0, 0, 2)))
      ),

      // splits
      (
        Branch(0, 1, 0),
        IdTree.Branch(IdTree.Branch(1, 0), 0),
        Branch(0, Branch(1, 1, 0), 0)
      ),
      (
        Branch(0, 1, 0),
        IdTree.Branch(IdTree.Branch(1, 0), IdTree.Branch(1, 0)),
        Branch(0, 1, Branch(0, 1, 0))
      )
    )

    // Test if inputs are not fillable and expectedEv is normalized
    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.fill(id) shouldBe ev
      isNormalized(expectedEv) shouldBe true
    }

    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.grow(id)._1 shouldBe expectedEv
    }

    // Also check if increment is correct
    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.increment(id) shouldBe expectedEv
    }
  }

  it should "prefer increments over branching" in {
    val eventsAndIds = Table(
      ("eventTree", "idTree", "expectedGrownEventTree"),
      (
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, 0, 1)),
        IdTree.Branch(IdTree.Branch(0, IdTree.Branch(0, 1)), IdTree.Branch(0, 1)),
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, 0, 2))
      ),
      (
        Branch(42, Branch(0, 0, Branch(0, 0, 1)), 1),
        IdTree.Branch(IdTree.Branch(0, IdTree.Branch(0, 1)), IdTree.Branch(0, 1)),
        Branch(42, Branch(0, 0, Branch(0, 0, 2)), 1)
      ),
      (
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, 0, 1)),
        IdTree.Branch(IdTree.Branch(0, IdTree.Branch(0, 1)), IdTree.Branch(0, 1)),
        Branch(0, Branch(0, 0, Branch(0, 0, 1)), Branch(0, 0, 2))
      )
    )

    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      assert(isNormalized(ev))
      assert(id.normalized == id)
      assert(isNormalized(expectedEv))
    }

    forAll(eventsAndIds) { (ev, id, expectedEv) =>
      ev.grow(id)._1 shouldBe expectedEv
    }
  }

  it should "return EventTree that is larger according to PartialOrdering" in {
    forAll(genEventTree.map(_.normalized), genIdTree.map(_.normalized).suchThat(!_.isAnonymous)) { (ev, id) =>
      val evFilled = ev.fill(id)
      eventTreePord.tryCompare(evFilled.grow(id)._1, evFilled) shouldBe Some(1)
      eventTreePord.tryCompare(evFilled, evFilled.grow(id)._1) shouldBe Some(-1)
    }
  }

  "increment" should "return normalized EventTree that is greater according to PartialOrdering" in {
    forAll(genEventTree, genIdTree.suchThat(!_.isAnonymous)) { (ev, id) =>
      val incEv = ev.increment(id)
      incEv shouldBe incEv.normalized
      eventTreePord.tryCompare(incEv, ev) shouldBe Some(1)
      eventTreePord.tryCompare(ev, incEv) shouldBe Some(-1)

      val incIncEv = incEv.increment(id)
      incIncEv shouldBe incIncEv.normalized
      eventTreePord.tryCompare(incIncEv, ev) shouldBe Some(1)
      eventTreePord.tryCompare(incIncEv, incEv) shouldBe Some(1)
      eventTreePord.tryCompare(incEv, incIncEv) shouldBe Some(-1)
    }
  }

  it should "throw an Exception when passing anonymous id" in {
    forAll(genEventTree) { ev =>
      assertThrows[IllegalArgumentException](ev.increment(IdTree.anonymous))
      assertThrows[IllegalArgumentException](ev.increment(IdTree.anonymous.split._1))
      assertThrows[IllegalArgumentException](ev.increment(IdTree.anonymous.split._2))
    }
  }

  it should "only fill event tree if possible" in {
    Branch(0, 1, 0).increment(1) shouldBe Leaf(1)
    Branch(0, 1, 0).increment(IdTree.Branch(0, 1)) shouldBe Leaf(1)
    Branch(0, Branch(0, 1, 1), 0).increment(IdTree.Branch(0, 1)) shouldBe Leaf(1)

    Branch(42, Branch(0, 0, 1), Branch(0, 0, 1))
      .increment(
        IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(1, 0))
      ) shouldBe Branch(42, Branch(0, 0, 1), 1)
  }

  it should "only fill event tree for successful fill examples" in {
    forAll(fillTestTable) { (ev, id, _) =>
      ev.increment(id) shouldBe ev.fill(id)
    }
  }

  it should "grow event tree if fill not possible and event tree not normalized" in {
    Branch(0, 1, 1).increment(1) shouldBe Leaf(2)
    Branch(1, 1, 1).increment(1) shouldBe Leaf(3)
  }

  it should "grow event tree if fill not possible" in {
    Leaf(1).increment(1) shouldBe Leaf(2)
    Branch(0, 1, Branch(0, 1, 0)).increment(IdTree.Branch(1, 0)) shouldBe Branch(0, 2, Branch(0, 1, 0))
  }

  val incomparableEventTrees: TableFor2[EventTree, EventTree] = Table(
    ("ev1", "ev2"),
    (Branch(0, 1, 0), Branch(0, 0, 1)),
    (Branch(2, 1, 0), Branch(2, 0, 1)),
    (Branch(2, 1, Branch(0, 0, 1)), Branch(2, 0, 1)),
    (Branch(2, Branch(0, 0, 1), 1), Branch(2, 1, 0)),
    (Branch(21, Branch(0, 0, 1), 0), Branch(21, Branch(0, 1, 0), Branch(21, 0, 1))),
    (Branch(0, 1, Branch(0, 0, Branch(0, 1, 0))), Branch(0, Branch(1, 0, Branch(1, 0, 2)), 0)),
    (Branch(21, Branch(0, 0, 1), Branch(20, 1, Branch(0, 0, 2))), Branch(21, 0, Branch(21, 0, 1))),
    (Leaf(22), Branch(21, 0, 2))
  )

  val leftSmallerEventTrees: TableFor2[EventTree, EventTree] = Table(
    ("ev1", "ev2"),
    (Leaf(0), Leaf(1)),
    (Leaf(0), Leaf(42)),
    (Leaf(21), Leaf(42)),
    (Leaf(21), Branch(21, 0, 42)),
    (Leaf(21), Branch(21, 42, 0)),
    (Leaf(21), Branch(21, Branch(0, 1, 0), 0)),
    (Leaf(21), Branch(21, Branch(0, 0, 1), 0)),
    (Leaf(21), Branch(21, 0, Branch(21, 0, 1))),
    (Branch(21, Branch(0, 0, 1), 0), Branch(21, Branch(0, 0, 1), Branch(21, 0, 1))),
    (Branch(21, 0, Branch(20, 1, Branch(0, 0, 2))), Branch(21, 0, Branch(21, 0, 1))),
    (Branch(21, Branch(20, 1, Branch(0, 0, 2)), Branch(0, 1, 0)), Branch(21, Branch(21, 0, 1), Branch(0, 1, 0))),
  )

  "PartialOrderingEventTree test data" should "be normalized" in {
    // Assumption of PartialOrdering
    forAll(incomparableEventTrees) { (ev1, ev2) =>
      ev1 shouldBe ev1.normalized
      ev2 shouldBe ev2.normalized
    }

    forAll(leftSmallerEventTrees) {(ev1,ev2) =>
      ev1 shouldBe ev1.normalized
      ev2 shouldBe ev2.normalized
    }
  }

  "PartialOrderingEventTree.lteq" should "work for incomparable EventTrees" in {
    forAll(incomparableEventTrees) { (ev1, ev2) =>
      ev1 <= ev2 shouldBe false
      ev2 <= ev1 shouldBe false
    }
  }

  it should "true when ev1 == ev2" in {
    forAll(genEventTree.map(_.normalized)) { (ev) =>
      eventTreePord.lteq(ev, ev) shouldBe true
    }
  }

  it should "be true when left smaller right and vice versa" in {
    forAll(leftSmallerEventTrees) { (ev1, ev2) =>
      assert(ev1 <= ev2)
      assert(!(ev2 <= ev1))
    }
  }

  "PartialOrderingEventTree.tryCompare" should "work for incomparable EventTrees" in {
    forAll(incomparableEventTrees) { (ev1, ev2) =>
      eventTreePord.tryCompare(ev1, ev2) shouldBe None
      eventTreePord.tryCompare(ev2, ev1) shouldBe None
    }
  }

  it should "be true when left smaller right and vice versa" in {
    forAll(leftSmallerEventTrees) { (ev1, ev2) =>
      eventTreePord.tryCompare(ev1, ev2) shouldBe Some(-1)
      eventTreePord.tryCompare(ev2, ev1) shouldBe Some(1)
    }
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

  it should "produce event trees that are normalized for generated EventTrees" in {
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
