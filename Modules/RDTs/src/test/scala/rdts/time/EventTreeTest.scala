package com.github.ckuessner
package causality

import causality.EventTree.{Branch, Leaf, seed, given}
import causality.EventTreeGenerators.{genEventTree, genEventTreeLeaf, genRandomEventTree}
import causality.IdTreeGenerators.genIdTree
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import scala.language.implicitConversions

class EventTreeTest extends FunSuite with ScalaCheckSuite {
  private val eventTreePord = EventTree.partialOrdering

  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMaxDiscardRatio(20)

  final def isNormalized(eventTree: EventTree): Boolean = eventTree match {
    case Leaf(_)                         => true
    case Branch(_, Leaf(0), r)           => isNormalized(r)
    case Branch(_, Branch(0, ll, lr), r) => isNormalized(ll) && isNormalized(lr) && isNormalized(r)
    case Branch(_, l, Leaf(0))           => isNormalized(l)
    case Branch(_, l, Branch(0, rl, rr)) => isNormalized(l) && isNormalized(rl) && isNormalized(rr)
    case _                               => false
  }

  test("Leaf only accepts positive values") {
    intercept[IllegalArgumentException](Leaf(-1))
    intercept[IllegalArgumentException](Leaf(-401))
  }

  test("Branch only accepts positive values") {
    intercept[IllegalArgumentException](Branch(-1, 23, 4))
    intercept[IllegalArgumentException](Branch(-1234, 0, 0))
  }

  test("min works for Leaf(n)") {
    assertEquals(seed.min, 0)
    assertEquals(Leaf(1).min, 1)
    assertEquals(Leaf(42).min, 42)
  }

  test("min works for Branch") {
    assertEquals(Branch(2, 1, 2).min, 3)
    assertEquals(Branch(2, Branch(1, 2, 4), 2).min, 4)
  }

  test("max works for Leaf(n)") {
    assertEquals(seed.max, 0)
    assertEquals(Leaf(1).max, 1)
    assertEquals(Leaf(42).max, 42)
  }

  test("max works for Branch") {
    assertEquals(Branch(2, 1, 2).max, 4)
    assertEquals(Branch(2, Branch(1, 2, 4), 2).max, 7)
  }

  test("lift works for Leaf") {
    assertEquals(Leaf(1).lift(1), Leaf(2))
    assertEquals(Leaf(2).lift(5), Leaf(7))
  }

  test("lift works for simple Branch") {
    assertEquals(Branch(1, 2, 2).lift(1), Branch(2, 2, 2))
    assertEquals(Branch(7, 0, 3).lift(4), Branch(11, 0, 3))
  }

  test("lift works for nested Branch") {
    assertEquals(Branch(1, Branch(2, 0, 4), 2).lift(1), Branch(2, Branch(2, 0, 4), 2))
    assertEquals(Branch(7, 0, Branch(2, 0, 4)).lift(4), Branch(11, 0, Branch(2, 0, 4)))
  }

  test("sink works for Leaf") {
    assertEquals(Leaf(1).sink(1), Leaf(0))
    assertEquals(Leaf(5).sink(2), Leaf(3))
  }

  test("sink works for Branch") {
    assertEquals(Branch(2, 4, 1).sink(2), Branch(0, 4, 1))
    assertEquals(Branch(2, 5, Branch(1, 2, 3)).sink(2), Branch(0, 5, Branch(1, 2, 3)))
  }

  test("sink refuses sinking beyond 0") {
    intercept[IllegalArgumentException](Leaf(1).sink(2))
  }

  test("join works for two Leafs") {
    assertEquals(Leaf(1) `join` Leaf(1), Leaf(1))
    assertEquals(Leaf(42) `join` Leaf(0), Leaf(42))
    assertEquals(Leaf(0) `join` Leaf(1), Leaf(1))
  }

  test("join works for Leaf and Branch") {
    assertEquals(Leaf(42) `join` Branch(2, 4, 5), Leaf(42))
    assertEquals(Branch(2, 4, 5) `join` Leaf(42), Leaf(42))
  }

  test("join produces normalized Ids") {
    forAll(genEventTree, genEventTreeLeaf) { (ev1, ev2) =>
      val joined = ev1 `join` ev2
      assertEquals(joined, joined.normalized)
    }
  }

  test("join is commutative") {
    forAll(genEventTree, genEventTree) { (ev1, ev2) =>
      assertEquals(ev1 `join` ev2, ev2 `join` ev1)
    }
  }

  for (ev, id) <- List(
        // ("eventTree", "idTree"),
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
  do {
    test(s"ev.fill(id) returns same reference for ev=$ev, id=$id") {
      assert(ev eq ev.fill(id))
    }
  }

  private val fillTestTable = List(
    // ("eventTree", "idTree", "expectedFilledEventTree"),
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

  for (ev, id, expectedEvFilled) <- fillTestTable do {
    test(s"fill simplifies the EventTree for ev=$ev, id=$id") {
      val evFilled = ev.fill(id)
      assertEquals(evFilled, expectedEvFilled)
      assertEquals(eventTreePord.tryCompare(evFilled, ev), Some(1))
      assertEquals(eventTreePord.tryCompare(ev, evFilled), Some(-1))
    }
  }

  test("fill returns same reference if equal to EventTree for generated EventTree and Id") {
    forAll(genEventTree.map(_.normalized), genIdTree.suchThat(!_.isAnonymous).map(_.normalized)) { (ev, id) =>
      val evFilled = ev.fill(id)
      evFilled == ev ==> assert(evFilled eq ev)
    }
  }

  test("fill produces a greater EventTree according to PartialOrdering") {
    forAll(genEventTree.map(_.normalized), genIdTree.suchThat(!_.isAnonymous).map(_.normalized)) { (ev, id) =>
      val evFilled = ev.fill(id)
      !(evFilled eq ev) ==> {
        assert(isNormalized(evFilled))
        assertEquals(eventTreePord.tryCompare(evFilled, ev), Some(1))
        assertEquals(eventTreePord.tryCompare(ev, evFilled), Some(-1))
      }
    }
  }

  for (ev, id, expectedEv) <- List(
        // ("eventTree", "idTree", "expectedGrownEventTree"),
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
  do {
    test(s"grow works for non-fillable examples and IdTree with single 1-Leaf for $ev, $id") {
      assertEquals(ev.grow(id)._1, expectedEv)
      assertEquals(ev.increment(id), expectedEv)
    }
  }

  test("grow works when needing to split a Leaf") {
    val eventsAndIds = List(
      // ("eventTree", "idTree", "expectedGrownEventTree"),
      (Leaf(0), IdTree.Branch(0, 1), Branch(0, 0, 1)),
      (Leaf(0), IdTree.Branch(1, 0), Branch(0, 1, 0)),
      (Leaf(1), IdTree.Branch(0, 1), Branch(1, 0, 1)),
      (Leaf(42), IdTree.Branch(1, 0), Branch(42, 1, 0)),
      (Branch(42, 0, 1), IdTree.Branch(0, IdTree.Branch(0, 1)), Branch(42, 0, Branch(1, 0, 1))),
      (Branch(42, 99, 0), IdTree.Branch(IdTree.Branch(1, 0), 0), Branch(42, Branch(99, 1, 0), 0))
    )

    eventsAndIds.foreach { (ev, id, _) =>
      assertEquals(ev.fill(id), ev)
    }

    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev.grow(id)._1, expectedEv)
    }

    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev.increment(id), expectedEv)
    }
  }

  test("grow prefers increment closest to root for IdTree with multiple 1-leafs") {
    val eventsAndIds = List(
      // ("eventTree", "idTree", "expectedGrownEventTree"),
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
    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev, ev.fill(id))
      assert(isNormalized(expectedEv))
    }

    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev.grow(id)._1, expectedEv)
    }

    // Also check if increment is correct
    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev.increment(id), expectedEv)
    }
  }

  test("grow prefers increments over branching") {
    val eventsAndIds = List(
      // ("eventTree", "idTree", "expectedGrownEventTree"),
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

    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assert(isNormalized(ev))
      assertEquals(id.normalized, id)
      assert(isNormalized(expectedEv))
    }

    eventsAndIds.foreach { (ev, id, expectedEv) =>
      assertEquals(ev.grow(id)._1, expectedEv)
    }
  }

  test("grow returns EventTree that is larger according to PartialOrdering") {
    forAll(genEventTree.map(_.normalized), genIdTree.map(_.normalized).suchThat(!_.isAnonymous)) { (ev, id) =>
      val evFilled = ev.fill(id)
      assertEquals(eventTreePord.tryCompare(evFilled.grow(id)._1, evFilled), Some(1))
      assertEquals(eventTreePord.tryCompare(evFilled, evFilled.grow(id)._1), Some(-1))
    }
  }

  test("increment returns normalized EventTree that is greater according to PartialOrdering") {
    forAll(genEventTree, genIdTree.suchThat(!_.isAnonymous)) { (ev, id) =>
      val incEv = ev.increment(id)
      assertEquals(incEv, incEv.normalized)
      assertEquals(eventTreePord.tryCompare(incEv, ev), Some(1))
      assertEquals(eventTreePord.tryCompare(ev, incEv), Some(-1))

      val incIncEv = incEv.increment(id)
      assertEquals(incIncEv, incIncEv.normalized)
      assertEquals(eventTreePord.tryCompare(incIncEv, ev), Some(1))
      assertEquals(eventTreePord.tryCompare(incIncEv, incEv), Some(1))
      assertEquals(eventTreePord.tryCompare(incEv, incIncEv), Some(-1))
    }
  }

  test("increment throws an exception when passing anonymous id") {
    forAll(genEventTree) { ev =>
      intercept[IllegalArgumentException](ev.increment(IdTree.anonymous))
      intercept[IllegalArgumentException](ev.increment(IdTree.anonymous.split._1))
      intercept[IllegalArgumentException](ev.increment(IdTree.anonymous.split._2)): Unit
    }
  }

  test("increment only fills event tree if possible") {
    assertEquals(Branch(0, 1, 0).increment(1), Leaf(1))
    assertEquals(Branch(0, 1, 0).increment(IdTree.Branch(0, 1)), Leaf(1))
    assertEquals(Branch(0, Branch(0, 1, 1), 0).increment(IdTree.Branch(0, 1)), Leaf(1))

    assertEquals(
      Branch(42, Branch(0, 0, 1), Branch(0, 0, 1))
        .increment(
          IdTree.Branch(IdTree.Branch(0, 1), IdTree.Branch(1, 0))
        ),
      Branch(42, Branch(0, 0, 1), 1)
    )
  }

  test("increment only fills event tree for successful fill examples") {
    fillTestTable.foreach { (ev, id, _) =>
      assertEquals(ev.increment(id), ev.fill(id))
    }
  }

  test("increment grows event tree if fill not possible and event tree not normalized") {
    assertEquals(Branch(0, 1, 1).increment(1), Leaf(2))
    assertEquals(Branch(1, 1, 1).increment(1), Leaf(3))
  }

  test("increment grows event tree if fill not possible") {
    assertEquals(Leaf(1).increment(1), Leaf(2))
    assertEquals(Branch(0, 1, Branch(0, 1, 0)).increment(IdTree.Branch(1, 0)), Branch(0, 2, Branch(0, 1, 0)))
  }

  private val incomparableEventTrees = List(
    // ("ev1", "ev2"),
    (Branch(0, 1, 0), Branch(0, 0, 1)),
    (Branch(2, 1, 0), Branch(2, 0, 1)),
    (Branch(2, 1, Branch(0, 0, 1)), Branch(2, 0, 1)),
    (Branch(2, Branch(0, 0, 1), 1), Branch(2, 1, 0)),
    (Branch(21, Branch(0, 0, 1), 0), Branch(21, Branch(0, 1, 0), Branch(21, 0, 1))),
    (Branch(0, 1, Branch(0, 0, Branch(0, 1, 0))), Branch(0, Branch(1, 0, Branch(1, 0, 2)), 0)),
    (Branch(21, Branch(0, 0, 1), Branch(20, 1, Branch(0, 0, 2))), Branch(21, 0, Branch(21, 0, 1))),
    (Leaf(22), Branch(21, 0, 2))
  )

  private val leftSmallerEventTrees = List(
    // ("ev1", "ev2"),
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

  test("PartialOrderingEventTree test data are normalized") {
    incomparableEventTrees.foreach { (ev1, ev2) =>
      assertEquals(ev1, ev1.normalized)
      assertEquals(ev2, ev2.normalized)
    }

    leftSmallerEventTrees.foreach { (ev1, ev2) =>
      assertEquals(ev1, ev1.normalized)
      assertEquals(ev2, ev2.normalized)
    }
  }

  test("PartialOrderingEventTree.lteq works for incomparable EventTrees") {
    incomparableEventTrees.foreach { (ev1, ev2) =>
      assert(!(ev1 <= ev2))
      assert(!(ev2 <= ev1))
    }
  }

  test("PartialOrderingEventTree.lteq is true when ev1 == ev2") {
    forAll(genEventTree.map(_.normalized)) { ev =>
      assert(eventTreePord.lteq(ev, ev))
    }
  }

  test("PartialOrderingEventTree.lteq is true when left smaller right and vice versa") {
    leftSmallerEventTrees.foreach { (ev1, ev2) =>
      assert(ev1 <= ev2)
      assert(!(ev2 <= ev1))
    }
  }

  test("PartialOrderingEventTree.tryCompare works for incomparable EventTrees") {
    incomparableEventTrees.foreach { (ev1, ev2) =>
      assertEquals(eventTreePord.tryCompare(ev1, ev2), None)
      assertEquals(eventTreePord.tryCompare(ev2, ev1), None)
    }
  }

  test("PartialOrderingEventTree.tryCompare is true when left smaller right and vice versa") {
    leftSmallerEventTrees.foreach { (ev1, ev2) =>
      assertEquals(eventTreePord.tryCompare(ev1, ev2), Some(-1))
      assertEquals(eventTreePord.tryCompare(ev2, ev1), Some(1))
    }
  }

  test("PartialOrderingEventTree.tryCompare is Some(0) when a == b") {
    forAll(genEventTree) { ev =>
      assertEquals(eventTreePord.tryCompare(ev, ev), Some(0))
    }
  }

  test("PartialOrderingEventTree.tryCompare is Some(1) or Some(-1) when a <= b xor b <= a") {
    forAll(genEventTree, genEventTree) { (left, right) =>
      ((left <= right) ^ (right <= left)) ==> {
        if left <= right then {
          assertEquals(eventTreePord.tryCompare(left, right), Some(-1))
          assertEquals(eventTreePord.tryCompare(right, left), Some(1))
        } else {
          assertEquals(eventTreePord.tryCompare(left, right), Some(1))
          assertEquals(eventTreePord.tryCompare(right, left), Some(-1))
        }
      }
    }
  }

  test("NormalForm[EventTree] works for Leaves") {
    forAll(Gen.posNum[Int]) { n =>
      assertEquals(Leaf(n).normalized, Leaf(n))
    }
  }

  test("NormalForm[EventTree] works for Branches") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int]) { (value: Int, leftLeafValue: Int, rightLeafValue: Int) =>
      val minLeaf             = Math.min(leftLeafValue, rightLeafValue)
      val normalizedEventTree = Branch(value, Leaf(leftLeafValue), Leaf(rightLeafValue)).normalized
      if leftLeafValue == rightLeafValue then {
        assertEquals(normalizedEventTree, Leaf(value + leftLeafValue))
      } else {
        assertEquals(
          normalizedEventTree,
          Branch(
            value + minLeaf,
            leftLeafValue - minLeaf,
            rightLeafValue - minLeaf
          )
        )
      }
    }
  }

  test("NormalForm[EventTree] works for nested examples") {
    val nestedEventTrees = List(
      // ("event", "expectedNormalForm"),
      Branch(0, Branch(0, 0, 0), Leaf(0))           -> Leaf(0),
      Branch(2, Branch(8, 9, 10), Branch(3, 3, 10)) -> Branch(8, Branch(11, 0, 1), Branch(0, 0, 7)),
      Branch(2, Branch(8, 0, 10), Branch(3, 3, 10)) -> Branch(8, Branch(2, 0, 10), Branch(0, 0, 7)),
      Branch(2, Leaf(3), Branch(3, 3, 10))          -> Branch(5, Leaf(0), Branch(3, 0, 7)),
      Branch(3, Branch(2, Branch(2, 2, 2), Leaf(4)), Branch(0, Leaf(6), Branch(0, 6, 6))) -> Leaf(9)
    )

    nestedEventTrees.foreach { (ev, expectedNormalForm) =>
      assertEquals(ev.normalized, expectedNormalForm)
    }
  }

  test("NormalForm[EventTree] produces normalized trees for generated EventTrees") {
    forAll(genEventTree) { ev =>
      assert(isNormalized(ev.normalized))
    }

    forAll(genRandomEventTree) { ev =>
      assert(isNormalized(ev.normalized))
    }
  }

  test("NormalForm[EventTree] returns this reference if already normalized") {
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
