package de.tu_darmstadt.stg.daimpl
package causality

import causality.EventTree.{Branch, Leaf, seed, given}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import scala.language.implicitConversions

class EventTreeTest extends AnyFlatSpec {
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
    // TODO: implement this
  }
}
