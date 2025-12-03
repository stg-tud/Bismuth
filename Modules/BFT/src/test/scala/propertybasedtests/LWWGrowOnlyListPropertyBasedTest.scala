package propertybasedtests

/*import datatypes.LWWGrowOnlyList
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LWWGrowOnlyListPropertyBasedTest extends munit.ScalaCheckSuite {

  val genOp: Gen[LWWGrowOnlyList[String] => LWWGrowOnlyList[String]] = {
      (c: LWWGrowOnlyList[String]) =>
        val i = Gen.choose(0, c.list.size).sample.get
        val s = Gen.alphaNumStr.sample.get
        c.add(i, s)

  }

  val genOps: Gen[List[LWWGrowOnlyList[String] => LWWGrowOnlyList[String]]] = Gen.listOf(genOp)

  def applyOps(c: LWWGrowOnlyList[String], ops: List[LWWGrowOnlyList[String] => LWWGrowOnlyList[String]]): LWWGrowOnlyList[String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a       = applyOps(LWWGrowOnlyList[String](), opsA)
      val b       = applyOps(LWWGrowOnlyList[String](), opsB)
      val merged1 = a.merge(b).list
      val merged2 = b.merge(a).list
      assertEquals(merged1, merged2)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(LWWGrowOnlyList[String](), opsA)
      val b = applyOps(LWWGrowOnlyList[String](), opsB)
      val c = applyOps(LWWGrowOnlyList[String](), opsC)
      assertEquals(a.merge(b).merge(c).list, a.merge(b.merge(c)).list)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LWWGrowOnlyList[String](), ops)
      assertEquals(a.merge(a).list, a.list)
    }
  }
}*/
