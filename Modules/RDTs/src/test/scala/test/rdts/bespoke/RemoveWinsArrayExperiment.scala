package test.rdts.bespoke

import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.RemoveWinsArray

import scala.language.implicitConversions
import rdts.experiments.RemoveWinsArrayExperiment

class RemoveWinsArrayExperimentTest extends munit.FunSuite {
  test("test") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArrayExperiment.empty[String]

    list = list `merge` list.append("a")(using aid)

    assertEquals(list.toList, List("a"))

    val list1A = list `merge` list.append("b")(using aid)
    val list1B = list `merge` list.append("c")(using bid)

    assertEquals(list1A.toList, List("a", "b"))
    assertEquals(list1B.toList, List("a", "c"))

    val list2 = list1A `merge` list1B
    assertEquals(list2.toList, List("a", "b", "c"))

    val list3 = list2 `merge` list2.apply(s => s.toUpperCase())(using aid)
    assertEquals(list3.toList, List("A", "B", "C"))

    val list4 = list3 `merge` list3.append("d")(using aid)
    // for-each should only apply to existing items, not newly appended ones
    assertEquals(list4.toList, List("A", "B", "C", "d"))

    val list5A = list4 `merge` list4.apply(s => s + "!")(using aid)
    val list5B = list4 `merge` list4.append("e")(using bid)

    assertEquals(list5A.toList, List("A!", "B!", "C!", "d!"))
    assertEquals(list5B.toList, List("A", "B", "C", "d", "e"))

    val list6 = list5A `merge` list5B
    assertEquals(list6.toList, List("A!", "B!", "C!", "d!", "e!"))
  }
}
