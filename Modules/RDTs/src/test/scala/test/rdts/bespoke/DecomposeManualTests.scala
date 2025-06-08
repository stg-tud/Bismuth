package test.rdts.bespoke

import rdts.base.LocalUid.asId
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.GrowOnlySet.{elements, insert}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, GrowOnlyMap, GrowOnlySet, LastWriterWins, MultiVersionRegister, PosNegCounter}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.UtilHacks.*

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

  test("Dotted[LastWriterWins[Int]] decomposition") {
    given bottomInt: Bottom[Int] with {
      override def empty: Int = 0
    }

    val empty: Dotted[LastWriterWins[Int]] = Bottom[Dotted[LastWriterWins[Int]]].empty

    val delta_1: Dotted[LastWriterWins[Int]] = empty.write(1)
    assertEquals(delta_1.read, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[LastWriterWins[Int]] = empty.write(2)
    assertEquals(delta_2.read, 2)

    val merged: Dotted[LastWriterWins[Int]] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.read, 2)

    val decomposed: Seq[Dotted[LastWriterWins[Int]]] =
      Decompose.decompose(merged)(using Dotted.decompose(using Decompose.atomic)).toSeq
    // LastWriterWins does not decompose, only returns the value.
    // Dotted decomposes context and value, but as LWW is not contextual, context is empty and not decomposed.
    assertEquals(decomposed.size, 1)
    assertEquals(decomposed.head.read, 2)
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

  test("Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] decomposition") {
    import GrowOnlyMap.given

    given stringOrdering: Ordering[String] = scala.math.Ordering.String

    given stringLattice: Lattice[String] = Lattice.fromOrdering(using stringOrdering)

    given HasDots[String] = HasDots.noDots

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

    val emptyMap: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      Dotted(Bottom[GrowOnlyMap[Int, LastWriterWins[String]]].empty)
    assertEquals(emptyMap.context.internal, Map.empty)

    val k1: Int    = 1
    val v1: String = "one"
    val e1         = LastWriterWins.now("")
    val delta_1: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k1, e1)(_.write(v1)))
    assertEquals(delta_1.context.internal, Map.empty)
    assertEquals(delta_1.data.keySet, Set(1))
    assertEquals(delta_1.data.get(1).map(_.payload), Some("one"))

    // delta_1 and delta_2 are in parallel

    val k2: Int    = 2
    val v2: String = "two"
    val e2         = LastWriterWins.now("")
    val delta_2: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k2, e2)(_.write(v2)))
    assertEquals(delta_2.context.internal, Map.empty)
    assertEquals(delta_2.data.keySet, Set(2))
    assertEquals(delta_2.data.get(2).map(_.payload), Some("two"))

    val merged: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      Lattice.merge(delta_1, delta_2)
    assertEquals(merged.context.internal, Map.empty)
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.payload), Some("one"))
    assertEquals(merged.data.get(2).map(_.payload), Some("two"))

    val decomposed: Seq[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      given Decompose[LastWriterWins[String]] = Decompose.atomic
      Decompose.decompose(merged).toSeq.sortBy(_.data.keys.headOption)
    // GrowOnlyMap decomposes every entry.
    // LastWriterWins does not decompose, only returns the value.
    // Dotted decomposes context and value, but as LWW is not contextual, the context is empty and there is no additional entry for it.
    assertEquals(decomposed.size, 2)

    assertEquals(decomposed(0).context.internal, Map.empty)
    assertEquals(decomposed(0).data.get(1).map(_.payload), Some("one"))

    assertEquals(decomposed(1).context.internal, Map.empty)
    assertEquals(decomposed(1).data.get(2).map(_.payload), Some("two"))
  }

}
