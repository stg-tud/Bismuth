/*import datatypes.LWWAWList
import org.scalacheck.*
import org.scalacheck.Prop.forAll

class LWWAWListPropertyBasedTests extends munit.ScalaCheckSuite {

  override def scalaCheckInitialSeed = "7-vlRWyVw0SBPiQBeG_cYTVr_gd6MJCKkwyl67lJGwP="

  val genOp: Gen[LWWAWList[String] => LWWAWList[String]] = {
    Gen.oneOf(
      (c: LWWAWList[String]) =>
        println("add")
        val i = Gen.choose(0, c.list.size).sample.get
        val s = Gen.alphaNumStr.sample.get
        println(s"($i, $s)")
        c.add(i, s),
      (c: LWWAWList[String]) =>
        if c.list.isEmpty then {
          println("add")
          val i = Gen.choose(0, c.list.size).sample.get
          val s = Gen.alphaNumStr.sample.get
          println(s"($i, $s)")
          c.add(i, s)
        } else {
          val i = Gen.choose(0, c.list.size - 1).sample.get
          println("remove")
          println(s"($i)")
          c.remove(i)
        }

    )
  }

  val genOps: Gen[List[LWWAWList[String] => LWWAWList[String]]] = Gen.listOf(genOp)

  def applyOps(c: LWWAWList[String], ops: List[LWWAWList[String] => LWWAWList[String]], k:String): LWWAWList[String] = {
    println(s"%%%  $k  %%%")
    ops.foldLeft(c)((cc, f) => cc.merge(f(cc)))
  }

  /*property("merge is commutative") {
    forAll(genOps, genOps) { (opsA, opsB) =>
      val a = applyOps(LWWAWList[String](), opsA)
      println("**************")
      val b = applyOps(LWWAWList[String](), opsB)
      val merged1 = a.merge(b).list
      val merged2 = b.merge(a).list
      assertEquals(merged1, merged2)
      println("-------------------")
    }
  }*/

  property("merge is associative") {
    forAll(genOps, genOps, genOps) { (opsA, opsB, opsC) =>
      println("//////////////////////////")
      val a = applyOps(LWWAWList[String](), opsA, "a")
      println("*****************************")
      val b = applyOps(LWWAWList[String](), opsB, "b")
      println("*****************************")
      val c = applyOps(LWWAWList[String](), opsC, "c")
      assertEquals((a.merge(b).merge(c)).list, a.merge(b.merge(c)).list)
    }
  }

  /*property("merge is idempotent") {
    forAll(genOps) { ops =>
      val a = applyOps(LWWAWList[String](), ops)
      assertEquals(a.merge(a).list, a.list)
    }
  }*/
}
*/
