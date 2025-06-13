package test.rdts.bespoke

import rdts.base.LocalUid.asId
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, MultiVersionRegister, PosNegCounter}

class DecomposeManualTests extends munit.ScalaCheckSuite {

  val r1: LocalUid = "r1".asId
  val r2: LocalUid = "r2".asId

  test("GrowOnlyCounter decomposition") {
    val empty: GrowOnlyCounter = Bottom[GrowOnlyCounter].empty

    val delta_1: GrowOnlyCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlyCounter = empty.inc()(using r2)
    assertEquals(delta_2.value, 1)

    val merged: GrowOnlyCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 2)

    val decomposed: Seq[GrowOnlyCounter] = Decompose.decompose(merged).toSeq
    // GrowOnlyCounter decomposes into every increment
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, 1)
    assertEquals(decomposed(1).value, 1)
  }

  test("PosNegCounter decomposition") {
    val empty: PosNegCounter = Bottom[PosNegCounter].empty

    val delta_1: PosNegCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: PosNegCounter = empty.dec()(using r2)
    assertEquals(delta_2.value, -1)

    val merged: PosNegCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 0)

    val decomposed: Seq[PosNegCounter] = Decompose.decompose(merged).toSeq.sortBy(_.value)
    // GrowOnlyCounter decomposes into every increment & decrement
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, -1)
    assertEquals(decomposed(1).value, 1)
  }

  test("MultiVersionRegister[Int] decomposition") {

    val empty: MultiVersionRegister[Int] = Bottom[MultiVersionRegister[Int]].empty

    val delta_1: MultiVersionRegister[Int] = empty.write(1)(using r1)
    assertEquals(delta_1.read, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: MultiVersionRegister[Int] = empty.write(2)(using r2)
    assertEquals(delta_2.read, Set(2))

    val merged: MultiVersionRegister[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.read, Set(1, 2))

    val decomposed: Seq[MultiVersionRegister[Int]] =
      Decompose.decompose(merged).toSeq.sortBy(_.read.headOption)
    // MultiVersionRegister decomposes every version
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).read, Set(1))
    assertEquals(decomposed(1).read, Set(2))
  }

  test("Set[Int] decomposition") {

    val empty: Set[Int] = Bottom[Set[Int]].empty

    val delta_1: Set[Int] = Set(1)
    assertEquals(delta_1, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: Set[Int] = Set(2)
    assertEquals(delta_2, Set(2))

    val merged: Set[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged, Set(1, 2))

    val decomposed: Seq[Set[Int]] =
      Decompose.decompose(merged).toSeq.sortBy(_.headOption)
    // Set decomposes every entry
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0), Set(1))
    assertEquals(decomposed(1), Set(2))
  }

}
