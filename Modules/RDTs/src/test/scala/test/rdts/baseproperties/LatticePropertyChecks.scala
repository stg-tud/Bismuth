package test.rdts.baseproperties

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Shrink}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.*
import rdts.experiments.AutomergyOpGraphLWW.OpGraph
import rdts.experiments.CausalStore
import rdts.time.{Dot, VectorClock}
import test.rdts.DataGenerator.ReplicatedListGen.given
import test.rdts.DataGenerator.{*, given}
import test.rdts.isGithubCi

// TODO, or maybe a note:
// These tests potentially fail if certain assumptions of the data types are invalidated by the generation strategy.
// Specifically, some values assume uniqueness, but the data generators don’t ensure uniqueness.
// Most notably, the Multi-Version-Register sometimes generates unique IDs associated with two different values, merging those is no longer commutative.
// This currently happens rarely enough, that a fix is postponed until a better strategy in general is found (just not allowing ID reuse might work, but would also exclude possible correct states, reducing the chance to find bugs. Though, that does not seem to be very high anyway …)

// class CausalQueueChecks       extends LatticePropertyChecks[Dotted[CausalQueue[ExampleData]]]
class OpGraphChecks              extends LatticePropertyChecks[OpGraph[ExampleData]]
class CausalStoreChecks          extends LatticePropertyChecks[CausalStore[Map[Dot, ExampleData]]]
class EnableWinsFlagChecks       extends LatticePropertyChecks[EnableWinsFlag]
class ConMultiVersionChecks      extends LatticePropertyChecks[MultiVersionRegister[Int]]
class GrowOnlyCounterChecks      extends LatticePropertyChecks[GrowOnlyCounter]
class IntChecks                  extends LatticePropertyChecks[Int]
class SetChecks                  extends LatticePropertyChecks[Set[String]]
class MapChecks                  extends LatticePropertyChecks[Map[String, Int]]
class OptionChecks               extends LatticePropertyChecks[Option[Int]]
class CusalLwwChecks             extends LatticePropertyChecks[LastWriterWins[Int]]
class LWWOptionChecks            extends LatticePropertyChecks[Option[LastWriterWins[Int]]]
class PosNegChecks               extends LatticePropertyChecks[PosNegCounter]
class TupleChecks                extends LatticePropertyChecks[(Set[Int], GrowOnlyCounter)]
class VectorClockChecks          extends LatticePropertyChecks[VectorClock]
class GrowOnlyListChecks         extends LatticePropertyChecks[GrowOnlyList[Int]](expensive = true)
class ReplicatedListChecks       extends LatticePropertyChecks[ReplicatedList[ExampleData]](expensive = true)
class ListAsVectorChecks         extends LatticePropertyChecks[List[Int]]
class LWWTupleChecks
    extends LatticePropertyChecks[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]

abstract class LatticePropertyChecks[A](
    expensive: Boolean = false,
    orderAgreesWithStructuralEquals: Boolean = true,
)(
    using
    arbitrary: Arbitrary[A],
    lattice: Lattice[A],
    shrink: Shrink[A],
) extends OrderTests(using Lattice.latticeOrder)(total = false, agreesWithEquals = orderAgreesWithStructuralEquals) {

  override def munitIgnore: Boolean = expensive && isGithubCi

  property("idempotent") {
    forAll { (a: A, b: A) =>
      val ab  = Lattice.merge(a, b)
      val abb = Lattice.merge(ab, b)
      assertEquals(ab, abb)

    }
  }
  property("commutative") {
    forAll { (a: A, b: A) =>
      assertEquals(Lattice.merge(b, a), Lattice.merge(a, b))
    }
  }
  property("associative") {
    forAll { (a: A, b: A, c: A) =>
      val ab   = Lattice.merge(a, b)
      val bc   = Lattice.merge(b, c)
      val ab_c = Lattice.merge(ab, c)
      val a_bc = Lattice.merge(a, bc)
      assertEquals(ab_c, a_bc, s"merge not equal, steps:\n  $ab\n  $bc")

      val bc_ab = bc `merge` ab
      assertEquals(bc_ab, ab_c, "variation on idempotent & commutative to work around insufficient test generators")
    }
  }

  property("merge agrees with order"):
    forAll: (left: A, right: A) =>
      val merged = left `merge` right

      assertEquals(left `merge` merged, merged, "naive lteq")
      assertEquals(right `merge` merged, merged, "naive lteq")
      assert(merged.subsumes(left), s"merged:\n  ${merged}\n ${left `merge` merged}")
      assert(merged.subsumes(right), s"merged:\n  ${merged}\n ${right `merge` merged}")
      assert(merged.inflates(left) || merged == Lattice.normalize(left), s"merged:\n  ${merged}")
      assert(merged.inflates(right) || merged == Lattice.normalize(right), s"merged:\n  ${merged}")

}
