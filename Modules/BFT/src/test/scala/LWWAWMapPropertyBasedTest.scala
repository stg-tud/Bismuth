import datatypes.LWWAWMap
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LWWAWMapPropertyBasedTest extends munit.ScalaCheckSuite {

  // override def scalaCheckInitialSeed = "idFJaoJzzr0D8GzFZij_htBSMt5EZy__9jlim_E-IeI="

  // override def scalaCheckInitialSeed = "eSAJE_66tBUZ5Pfuv9XBvIus6vLz-6VtsdcUstdP_6E="

  // override def scalaCheckInitialSeed = "KtTZ42SL7mcctkxBVRBmIfGbHFi39eQMCCGroGLjUfH="

  val genOp: Gen[LWWAWMap[String, String] => LWWAWMap[String, String]] = {
    Gen.oneOf(
      (map: LWWAWMap[String, String]) =>
          val key = Gen.alphaUpperStr.sample.get.take(3)
          val v   = Gen.alphaLowerStr.sample.get.take(3)
          // println(s"put: ($key, $v)")
          map.put(key, v)
      ,
      (map: LWWAWMap[String, String]) =>
          val key = Gen.alphaUpperStr.sample.get.take(3)
          // println(s"remove: $key")
          map.remove(key)
    )
  }

  val genOps: Gen[List[LWWAWMap[String, String] => LWWAWMap[String, String]]] = Gen.listOf(genOp)

  def applyOps(
      c: LWWAWMap[String, String],
      ops: List[LWWAWMap[String, String] => LWWAWMap[String, String]]
  ): LWWAWMap[String, String] =
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      // println("***********************************************")
      val a = applyOps(LWWAWMap[String, String](), opsA)
      // println("//////////////////////////////////////////////")
      val b  = applyOps(LWWAWMap[String, String](), opsB)
      val m1 = a.merge(b).map
      val m2 = b.merge(a).map
      // println(m1)
      // println(m2)
      assertEquals(m1, m2)
      // println("********************")
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
