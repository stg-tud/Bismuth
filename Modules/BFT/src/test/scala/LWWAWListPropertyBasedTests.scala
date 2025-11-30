import datatypes.LWWAWList
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LWWAWListPropertyBasedTests extends munit.ScalaCheckSuite {

  val genOp: Gen[LWWAWList[String] => LWWAWList[String]] = {
    Gen.oneOf(
      (c: LWWAWList[String]) =>
        val i = Gen.choose(0, c.list.size).sample.get
        val s = Gen.alphaNumStr.sample.get
        c.add(i, s),
      (c: LWWAWList[String]) =>
        if c.list.isEmpty then {
          val i = Gen.choose(0, c.list.size).sample.get
          val s = Gen.alphaNumStr.sample.get
          c.add(i, s)
        } else {
          val i = Gen.choose(0, c.list.size - 1).sample.get
          c.remove(i)
        }

    )
  }

  val genOps: Gen[List[LWWAWList[String] => LWWAWList[String]]] = Gen.listOf(genOp)

  def applyOps(c: LWWAWList[String], ops: List[LWWAWList[String] => LWWAWList[String]]): LWWAWList[String] = {
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))
  }

  property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(LWWAWList[String](), opsA)
      val b = applyOps(LWWAWList[String](), opsB)
      val merged1 = a.merge(b).list
      val merged2 = b.merge(a).list
      assertEquals(merged1, merged2)
    }
  }

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      val a = applyOps(LWWAWList[String](), opsA)
      val b = applyOps(LWWAWList[String](), opsB)
      val c = applyOps(LWWAWList[String](), opsC)
      assertEquals((a.merge(b).merge(c)).list, a.merge(b.merge(c)).list)
    }
  }

  property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LWWAWList[String](), ops)
      assertEquals(a.merge(a).list, a.list)
    }
  }
}

