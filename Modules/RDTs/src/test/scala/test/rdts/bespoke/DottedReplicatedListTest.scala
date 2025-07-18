package test.rdts.bespoke

import rdts.base.LocalUid
import rdts.experiments.DottedReplicatedList

import scala.util.Random

class DottedReplicatedListTest extends munit.FunSuite {
  test("yoink") {
    given LocalUid = LocalUid.gen()
    val random = Random(0)
    val res = (1 to 1000).foldLeft(DottedReplicatedList.empty) { (l, i) =>
      val pos =  random.nextInt(l.size + 1)
      l `merge` l.insertAt(pos, i.toString)
    }

    println(res)

  }
}
