package propertybasedtests

import datatypes.EnableWinsFlag
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class EnableWinsFlagPropertyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[EnableWinsFlag => EnableWinsFlag] =
    Gen.oneOf(
      (c: EnableWinsFlag) => c.enable,
      (c: EnableWinsFlag) => c.disable,
    )

  val genOps: Gen[List[EnableWinsFlag => EnableWinsFlag]] = Gen.listOf(genOp)

  def applyOps(c: EnableWinsFlag, ops: List[EnableWinsFlag => EnableWinsFlag]): EnableWinsFlag =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(EnableWinsFlag(), opsA)
      val b = applyOps(EnableWinsFlag(), opsB)

      assertEquals(a.merge(b).read, b.merge(a).read)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(EnableWinsFlag(), opsA)
      val b = applyOps(EnableWinsFlag(), opsB)
      val c = applyOps(EnableWinsFlag(), opsC)
      assertEquals(a.merge(b).merge(c).read, a.merge(b.merge(c)).read)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(EnableWinsFlag(), ops)
      assertEquals(a.merge(a).read, a.read)
    }
  }
}
