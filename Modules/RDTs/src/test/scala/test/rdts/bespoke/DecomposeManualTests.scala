package test.rdts.bespoke

import rdts.base.LocalUid.asId
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.GrowOnlySet.{elements, insert}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, GrowOnlyMap, GrowOnlySet, LastWriterWins, MultiVersionRegister, PosNegCounter}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}

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

    val delta_1: MultiVersionRegister[Int] = empty.write(using r1)(1)
    assertEquals(delta_1.read, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: MultiVersionRegister[Int] = empty.write(using r2)(2)
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

  test("GrowOnlySet[Int] decomposition") {
    import GrowOnlySet.given

    val empty: GrowOnlySet[Int] = Bottom[GrowOnlySet[Int]].empty

    val delta_1: GrowOnlySet[Int] = empty.insert(1)
    assertEquals(delta_1.elements, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlySet[Int] = empty.insert(2)
    assertEquals(delta_2.elements, Set(2))

    val merged: GrowOnlySet[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.elements, Set(1, 2))

    val decomposed: Seq[GrowOnlySet[Int]] =
      Decompose.decompose(merged).toSeq.sortBy(_.elements.headOption)
    // GrowOnlySet decomposes every entry
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).elements, Set(1))
    assertEquals(decomposed(1).elements, Set(2))
  }

}
