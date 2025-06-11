package test.rdts.baseproperties

import munit.TestValues
import munit.internal.FutureCompat.ExtensionFuture
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Shrink}
import rdts.base.{Bottom, BottomOpt, Decompose, Lattice}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, GrowOnlyList, LastWriterWins, MultiVersionRegister, PosNegCounter, ReplicatedList}
import rdts.time.{Dot, Dots}
import test.rdts.DataGenerator.RGAGen.given
import test.rdts.DataGenerator.{*, given}
import test.rdts.isGithubCi

import scala.util.{Failure, Success}

class EnableWinsFlagDecomposeChecks  extends DecomposePropertyChecks[EnableWinsFlag]
class ConMultiVersionDecomposeChecks extends DecomposePropertyChecks[MultiVersionRegister[Int]]
class GrowOnlyCounterDecomposeChecks extends DecomposePropertyChecks[GrowOnlyCounter]
class IntDecomposeChecks             extends DecomposePropertyChecks[Int]
class SetDecomposeChecks             extends DecomposePropertyChecks[Set[String]]
class MapDecomposeChecks             extends DecomposePropertyChecks[Map[String, Int]]
class PosNegDecomposeChecks          extends DecomposePropertyChecks[PosNegCounter]
class TupleDecomposeChecks           extends DecomposePropertyChecks[(Set[Int], GrowOnlyCounter)]
class GrowOnlyListDecomposeChecks    extends DecomposePropertyChecks[GrowOnlyList[Int]](expensive = true)
class ReplicatedListDecomposeChecks
    extends DecomposePropertyChecks[ReplicatedList[ExampleData]](expensive = true)
class LWWTupleDecomposeChecks
    extends LatticePropertyChecks[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]

abstract class DecomposePropertyChecks[A](
    expensive: Boolean = false,
    flaky: Boolean = false
)(
    using
    arbitrary: Arbitrary[A],
    lattice: Lattice[A],
    decompose: Decompose[A],
    bottomOpt: BottomOpt[A],
    shrink: Shrink[A],
) extends munit.ScalaCheckSuite {

  override def munitIgnore: Boolean = expensive && isGithubCi

  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms ++ List(
    new TestTransform(
      "flakyTestGenarators",
      { t =>
        if !(flaky && isGithubCi) then t
        else
          t.withBodyMap(_.transformCompat {
            case Failure(exception) => Success(new TestValues.FlakyFailure(exception))
            case succ               => succ
          }(using munitExecutionContext))
      }
    )
  )

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed
      val normalized = Lattice.normalize(theValue)

      decomposed.foreach { d =>
        assertEquals(
          d `merge` theValue,
          normalized,
          s"naive order broken:\n ${d}\n $theValue\n${decomposed.mkString("   ", "\n   ", "\n")}"
        )
        assert(
          Lattice.subsumption(d, normalized),
          s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d `merge` normalized}"
        )
        if decomposed.sizeIs > 1
        then
          BottomOpt.explicit: bo =>
            assertNotEquals(bo.empty, d, s"decomposed result was empty\n  $decomposed")
      }

      BottomOpt.explicit: bo =>
        assertEquals(bo.empty `merge` theValue, normalized, "bottom is bottom")

      val merged =
        if decomposed.sizeIs == 1
        then Some(Lattice.normalize(decomposed.head))
        else decomposed.reduceLeftOption(Lattice.merge)

      assertEquals(
        merged.orElse(BottomOpt.explicit(_.empty)),
        Some(normalized),
        s"decompose does not recompose (test may require a bottom instance if any component decomposes into None)"
      )

    }
  }
}
