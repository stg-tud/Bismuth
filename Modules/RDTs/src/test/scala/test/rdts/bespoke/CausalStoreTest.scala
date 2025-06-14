package test.rdts.bespoke

import rdts.base.{Bottom, Lattice, Uid}
import rdts.experiments.CausalStore.CausalDelta
import rdts.experiments.{BoundedCounter, CausalStore}
import rdts.time.{ArrayRanges, Dot, Dots}
import test.rdts.DataGenerator.ExampleData
import test.rdts.{TestReplica, given}

import scala.language.implicitConversions

class CausalStoreTest extends munit.FunSuite {

  test("basic usage") {

    val a: CausalStore[Map[Dot, ExampleData]] =
      CausalStore(
        Set(CausalDelta(
          Dot(Uid.predefined("c"), 1),
          Dots(Map()),
          Map(Dot("c", 1) -> "A pen Chris")
        )),
        Dots.single(Dot("a", 2)),
        Some(Map(Dot("a", 2) -> "A in Anne"))
      )

    val b: CausalStore[Map[Dot, ExampleData]] =
      CausalStore(
        Set(CausalDelta(
          Dot(Uid.predefined("d"), 3),
          Dots(Map()),
          Map(Dot("d", 3) -> "B pen Erin")
        )),
        Dots(Map(Uid.predefined("d") -> ArrayRanges.elems(3), Uid.predefined("g") -> ArrayRanges.elems(4))),
        Some(Map(Dot("g", 4) -> "B in Taylor"))
      )

    val c: CausalStore[Map[Dot, ExampleData]] =
      CausalStore(
        Set(),
        Dots(Map(
          Uid.predefined("d") -> ArrayRanges.elems(3),
        )),
        None
      )

    val ab = a `merge` b
    val bc = b `merge` c

    val ab_c = ab `merge` c
    val a_bc = a `merge` bc

    assertEquals(ab_c, a_bc)
  }

  test("compacts") {
    val a: CausalStore[Map[Dot, ExampleData]] =
      CausalStore(
        Set(CausalDelta(
          Dot(Uid.predefined("c"), 1),
          Dots(Map()),
          Map(Dot("c", 1) -> "A pen Chris")
        )),
        Dots.single(Dot("a", 2)),
        Some(Map(Dot("a", 2) -> "A in Anne"))
      )

    val norm = Lattice.normalize(a)
    assertEquals(norm.pending, Set.empty)
  }

}
