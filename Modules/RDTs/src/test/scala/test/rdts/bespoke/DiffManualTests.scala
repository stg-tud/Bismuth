package test.rdts.bespoke

import rdts.base.LocalUid.asId
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.GrowOnlySet.{elements, insert}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, GrowOnlyMap, GrowOnlySet, LastWriterWins, MultiVersionRegister, PosNegCounter}
import rdts.dotted.{Dotted, HasDots}

class DiffManualTests extends munit.ScalaCheckSuite {

  val r1: LocalUid = "r1".asId
  val r2: LocalUid = "r2".asId

  test("GrowOnlyCounter diff") {
    val empty: GrowOnlyCounter = Bottom[GrowOnlyCounter].empty

    val delta_1: GrowOnlyCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlyCounter = empty.inc()(using r2)
    assertEquals(delta_2.value, 1)

    val merged: GrowOnlyCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 2)

    val delta_1_diff_delta_2: Option[GrowOnlyCounter] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[GrowOnlyCounter] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[GrowOnlyCounter] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[GrowOnlyCounter] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("PosNegCounter diff") {
    val empty: PosNegCounter = Bottom[PosNegCounter].empty

    val delta_1: PosNegCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: PosNegCounter = empty.dec()(using r2)
    assertEquals(delta_2.value, -1)

    val merged: PosNegCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 0)

    val delta_1_diff_delta_2: Option[PosNegCounter] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[PosNegCounter] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[PosNegCounter] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[PosNegCounter] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("EnableWinsFlag diff") {
    val empty: EnableWinsFlag = Bottom[EnableWinsFlag].empty

    val delta_1: EnableWinsFlag = empty.enable(using r1)()
    assertEquals(delta_1.read, true)

    // delta_1 and delta_2 are in parallel

    val delta_2: EnableWinsFlag = empty.disable()
    assertEquals(delta_2.read, false)

    val merged: EnableWinsFlag = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.read, true)

    val delta_1_diff_delta_2: Option[EnableWinsFlag] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, false, "delta_1 wins - delta_2 is obsolete")

    val delta_2_diff_delta_1: Option[EnableWinsFlag] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[EnableWinsFlag] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[EnableWinsFlag] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("MultiVersionRegister[Int] diff") {

    val empty: MultiVersionRegister[Int] = Bottom[MultiVersionRegister[Int]].empty

    val delta_1: MultiVersionRegister[Int] = empty.write(using r1)(1)
    assertEquals(delta_1.read, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: MultiVersionRegister[Int] = empty.write(using r2)(2)
    assertEquals(delta_2.read, Set(2))

    val merged: MultiVersionRegister[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.read, Set(1, 2))

    val delta_1_diff_delta_2: Option[MultiVersionRegister[Int]] =
      Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[MultiVersionRegister[Int]] =
      Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[MultiVersionRegister[Int]] =
      Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[MultiVersionRegister[Int]] =
      Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("GrowOnlySet[Int] diff") {
    import GrowOnlySet.given

    val empty: GrowOnlySet[Int] = Bottom[GrowOnlySet[Int]].empty

    val delta_1: GrowOnlySet[Int] = empty.insert(1)
    assertEquals(delta_1.elements, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlySet[Int] = empty.insert(2)
    assertEquals(delta_2.elements, Set(2))

    val merged: GrowOnlySet[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.elements, Set(1, 2))

    val delta_1_diff_delta_2: Option[GrowOnlySet[Int]] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[GrowOnlySet[Int]] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[GrowOnlySet[Int]] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[GrowOnlySet[Int]] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

}
