import datatypes.LWWAWMap
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LWWAWMapPropertyBasedTest extends munit.ScalaCheckSuite {

  val genOp: Gen[LWWAWMap[String, String] => LWWAWMap[String, String]] = {
    Gen.oneOf(
      (map: LWWAWMap[String, String]) => map.put(Gen.alphaNumStr.sample.get, Gen.alphaNumStr.sample.get)
      ,
      (map: LWWAWMap[String, String]) => map.remove(Gen.alphaNumStr.sample.get)
    )
  }

  val genOps: Gen[List[LWWAWMap[String, String] => LWWAWMap[String, String]]] = Gen.listOf(genOp)

  def applyOps(c: LWWAWMap[String, String], ops: List[LWWAWMap[String, String] => LWWAWMap[String, String]]): LWWAWMap[String, String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a       = applyOps(LWWAWMap[String, String](), opsA)
      val b       = applyOps(LWWAWMap[String, String](), opsB)
      assertEquals(a.merge(b).map, b.merge(a).map)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(LWWAWMap[String, String](), opsA)
      val b = applyOps(LWWAWMap[String, String](), opsB)
      val c = applyOps(LWWAWMap[String, String](), opsC)
      assertEquals(a.merge(b).merge(c).map, a.merge(b.merge(c)).map)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LWWAWMap[String, String](), ops)
      assertEquals(a.merge(a).map, a.map)
    }
  }
}
