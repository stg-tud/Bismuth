package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.RemoveWinsArray

import scala.language.implicitConversions

class RemoveWinsArrayTest extends munit.FunSuite {
  test("insert") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    val v0 = RemoveWinsArray.empty[String]

    val v1 = v0 `merge` v0.insert(0, "a")(using aid)
    assertEquals(v1.toList, List("a"))

    val delta1A = v1.insert(0, "b")(using aid)
    val delta1B = v1.insert(1, "c")(using bid)

    val v2a = v1 `merge` delta1A
    val v2b = v1 `merge` delta1B

    assertEquals(v2a.toList, List("b", "a"))
    assertEquals(v2b.toList, List("a", "c"))

    val v3 = v1 `merge` delta1A `merge` delta1B
    assertEquals(v3.toList, List("b", "a", "c"))

    val delta2A = v3.insert(0, "d")(using aid)
    val delta2B = v3.insert(0, "e")(using bid)

    val v4a = v3 `merge` delta2A
    val v4b = v3 `merge` delta2B

    assertEquals(v4a.toList, List("d", "b", "a", "c"))
    assertEquals(v4b.toList, List("e", "b", "a", "c"))

    val v5 = v3 `merge` delta2A `merge` delta2B
    assertEquals(v5.toList, List("d", "e", "b", "a", "c"))
  }

  test("delete") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    list = list `merge` list.append("e")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d", "e"))

    val delta1a = list.remove(0)
    val delta1b = list.remove(0)
    val v1a     = list `merge` delta1a
    val v1b     = list `merge` delta1b
    assertEquals(v1a.toList, List("b", "c", "d", "e"))
    assertEquals(v1b.toList, List("b", "c", "d", "e"))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList, List("b", "c", "d", "e"))

    list = list `merge` list.remove(3)
    assertEquals(list.toList, List("b", "c", "d"))
  }

  test("appendAll") {
    val aid = Uid.predefined("a")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.prepend("a")(using aid)
    list = list `merge` list.append("g")(using aid)
    assertEquals(list.toList, List("a", "g"))

    list = list `merge` list.insertAll(1, Iterable("b", "c", "d", "e", "f"))(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d", "e", "f", "g"))
  }

  test("update") {
    val aid = Uid.predefined("a")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.append("a")(using aid)
    assertEquals(list.toList, List("a"))

    list = list `merge` list.update(0, "b")(using aid)
    assertEquals(list.toList, List("b"))

    list = list `merge` list.update(1, "c")(using aid)
    assertEquals(list.toList, List("b", "c"))
  }

  test("move") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d"))

    val delta1a = list.move(0, 2)(using aid)
    val delta1b = list.move(0, 1)(using bid)

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList, List("b", "c", "a", "d"))
    assertEquals(v1b.toList, List("b", "a", "c", "d"))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList, List("b", "a", "c", "d"))
  }

  test("nested move") {
    // Move an element on one replica, and move another element next to it to it on another replica

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]
    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    list = list `merge` list.append("e")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d", "e"))

    val delta1a = list.move(4, 2)(using aid)
    val delta1b = list.move(1, 5)(using bid)

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList, List("a", "b", "e", "c", "d"))
    assertEquals(v1b.toList, List("a", "c", "d", "e", "b"))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList, List("a", "e", "c", "d", "b"))
  }
}
