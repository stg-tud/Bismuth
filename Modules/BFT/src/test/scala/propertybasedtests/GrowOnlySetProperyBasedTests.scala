package propertybasedtests

import datatypes.GrowOnlySet
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class GrowOnlySetProperyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[GrowOnlySet[String] => GrowOnlySet[String]] =
    (c: GrowOnlySet[String]) => c.add(Gen.alphaNumStr.sample.get)

  val genOps: Gen[List[GrowOnlySet[String] => GrowOnlySet[String]]] = Gen.listOf(genOp)

  def applyOps(c: GrowOnlySet[String], ops: List[GrowOnlySet[String] => GrowOnlySet[String]]): GrowOnlySet[String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(GrowOnlySet[String](), opsA)
      val b = applyOps(GrowOnlySet[String](), opsB)

      assertEquals(a.merge(b).elements, b.merge(a).elements)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(GrowOnlySet[String](), opsA)
      val b = applyOps(GrowOnlySet[String](), opsB)
      val c = applyOps(GrowOnlySet[String](), opsC)
      assertEquals(a.merge(b).merge(c).elements, a.merge(b.merge(c)).elements)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(GrowOnlySet[String](), ops)
      assertEquals(a.merge(a).elements, a.elements)
    }
  }
}
