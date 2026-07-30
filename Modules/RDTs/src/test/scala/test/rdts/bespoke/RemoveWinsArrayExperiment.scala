package test.rdts.bespoke

import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.RemoveWinsArray

import scala.language.implicitConversions
import rdts.experiments.RemoveWinsArrayExperiment

class RemoveWinsArrayExperimentTest extends munit.FunSuite {
  given Lattice[String] = Lattice.assertEquals

  test("test") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArrayExperiment.empty[String]

    list = list `merge` list.append("a")(using aid)

    assertEquals(list.toList, List("a"))

    val delta1A = list.append("b")(using aid)
    val delta1B = list.append("c")(using bid)
    val list1A  = list `merge` delta1A
    val list1B  = list `merge` delta1B

    assertEquals(list1A.toList, List("a", "b"))
    assertEquals(list1B.toList, List("a", "c"))

    val list2 = list `merge` delta1A `merge` delta1B
    assertEquals(list2.toList, List("a", "b", "c"))

    val list3 = list2 `merge` list2.apply(s => s.toUpperCase())(using aid)
    assertEquals(list3.toList, List("A", "B", "C"))

    val list4 = list3 `merge` list3.append("d")(using aid)
    assertEquals(list4.toList, List("A", "B", "C", "d"))

    val delta5A = list4.apply(s => s + "!")(using aid)
    val delta5B = list4.appendAll(Set("e", "f"))(using bid)
    val list5A  = list4 `merge` delta5A
    val list5B  = list4 `merge` delta5B

    assertEquals(list5A.toList, List("A!", "B!", "C!", "d!"))
    assertEquals(list5B.toList, List("A", "B", "C", "d", "e", "f"))

    // for-each should apply to concurrently inserted items
    val list6 = list4 `merge` delta5A `merge` delta5B
    assertEquals(list6.toList, List("A!", "B!", "C!", "d!", "e!", "f!"))

    val delta7A = list6.apply(s => s.reverse)(using aid)
    val delta7B = list6.apply(s => s.toLowerCase())(using bid)
    val list7A  = list6 `merge` delta7A
    val list7B  = list6 `merge` delta7B

    assertEquals(list7A.toList, List("!A", "!B", "!C", "!d", "!e", "!f"))
    assertEquals(list7B.toList, List("a!", "b!", "c!", "d!", "e!", "f!"))

    val list7 = list6 `merge` delta7A `merge` delta7B
    assertEquals(list7.toList, List("!a", "!b", "!c", "!d", "!e", "!f"))
  }
}
