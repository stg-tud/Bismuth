package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.RemoveWinsArray

import scala.language.implicitConversions
import rdts.base.Lattice
import rdts.datatypes.LastWriterWins as LWW

class RemoveWinsArrayTest extends munit.FunSuite {
  test("insert") {
    given Lattice[String] = Lattice.assertEquals

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
    given Lattice[String] = Lattice.assertEquals

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
    given Lattice[String] = Lattice.assertEquals

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

    var list = RemoveWinsArray.empty[LWW[String]]

    list = list `merge` list.append(LWW.now("a"))(using aid)
    assertEquals(list.toList.map(_.value), List("a"))

    list = list `merge` list.update(0, LWW.now("b"))
    assertEquals(list.toList.map(_.value), List("b"))

    list = list `merge` list.update(1, LWW.now("c"))
    assertEquals(list.toList.map(_.value), List("b"))
  }

  test("move") {
    given Lattice[String] = Lattice.assertEquals

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d"))

    val delta1a = list.move(0, 3)(using aid)
    val delta1b = list.move(0, 2)(using bid)

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList, List("b", "c", "a", "d"))
    assertEquals(v1b.toList, List("b", "a", "c", "d"))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList, List("b", "a", "c", "d"))
  }

  test("nested move") {
    // Move an element on one replica, and move another element next to it to it on another replica
    given Lattice[String] = Lattice.assertEquals

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

  test("moveRange") {
    given Lattice[String] = Lattice.assertEquals

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]

    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    list = list `merge` list.append("e")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d", "e"))

    // Move range [1, 3) (elements "b", "c") to position 4
    list = list `merge` list.moveRange(1, 3, 4)(using aid)
    assertEquals(list.toList, List("a", "d", "b", "c", "e"))

    // Move range [0, 2) (elements "a", "d") to position 5
    list = list `merge` list.moveRange(0, 2, 5)(using aid)
    assertEquals(list.toList, List("b", "c", "e", "a", "d"))

    // Move range [2, 5) (elements "e", "a", "d") to position 0
    list = list `merge` list.moveRange(2, 5, 0)(using aid)
    assertEquals(list.toList, List("e", "a", "d", "b", "c"))
  }

  test("concurrent moveRange") {
    given Lattice[String] = Lattice.assertEquals

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]
    list = list `merge` list.append("a")(using aid)
    list = list `merge` list.append("b")(using aid)
    list = list `merge` list.append("c")(using aid)
    list = list `merge` list.append("d")(using aid)
    list = list `merge` list.append("e")(using aid)
    list = list `merge` list.append("f")(using aid)
    assertEquals(list.toList, List("a", "b", "c", "d", "e", "f"))

    // Replica A moves range [1, 3) to position 6
    val delta1a = list.moveRange(1, 3, 6)(using aid)
    // Replica B moves range [3, 5) to position 0
    val delta1b = list.moveRange(3, 5, 0)(using bid)

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList, List("a", "d", "e", "f", "b", "c"))
    assertEquals(v1b.toList, List("d", "e", "a", "b", "c", "f"))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList, List("d", "e", "a", "f", "b", "c"))
  }

  test("move subrange into range being moved") {
    given Lattice[String] = Lattice.assertEquals

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[String]
    list = list `merge` list.append("Water plants")(using aid)
    list = list `merge` list.append("Buy juice")(using aid)
    list = list `merge` list.append("Call doctor")(using aid)
    list = list `merge` list.append("Do laundry")(using aid)
    list = list `merge` list.append("Cook dinner")(using aid)
    assertEquals(list.toList, List("Water plants", "Buy juice", "Call doctor", "Do laundry", "Cook dinner"))

    val delta1a = list.moveRange(1, 3, 0)(using aid)

    val delta1b = list.moveRange(3, 5, 2)(using bid)

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList, List("Buy juice", "Call doctor", "Water plants", "Do laundry", "Cook dinner"))
    assertEquals(v1b.toList, List("Water plants", "Buy juice", "Do laundry", "Cook dinner", "Call doctor"))

    list = list `merge` delta1b `merge` delta1a

    // Because of the way positions are generated, we expect the original target indices to be remained
    assertEquals(list.toList, List("Buy juice", "Call doctor", "Water plants", "Do laundry", "Cook dinner"))
  }

  test("nested list with value updates") {
    given Lattice[String] = Lattice.assertEquals

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var list = RemoveWinsArray.empty[RemoveWinsArray[String]]

    list = list `merge` list.append(RemoveWinsArray.of("x")(using aid))(using aid)
    list = list `merge` list.append(RemoveWinsArray.of("y")(using aid))(using aid)
    list = list `merge` list.append(RemoveWinsArray.of("z")(using aid))(using aid)
    assertEquals(
      list.toList.map(_.toList),
      List(List("x"), List("y"), List("z"))
    )

    // Update nested lists concurrently
    val delta1a = list.updateWith(0, _.append("a")(using aid))
    val delta1b = list.updateWith(0, _.append("b")(using bid))

    val v1a = list `merge` delta1a
    val v1b = list `merge` delta1b
    assertEquals(v1a.toList.map(_.toList), List(List("x", "a"), List("y"), List("z")))
    assertEquals(v1b.toList.map(_.toList), List(List("x", "b"), List("y"), List("z")))

    list = list `merge` delta1a `merge` delta1b
    assertEquals(list.toList.map(_.toList), List(List("x", "a", "b"), List("y"), List("z")))

    // Update another position
    list = list `merge` list.updateWith(1, _.append("modified")(using aid))
    assertEquals(list.toList.map(_.toList), List(List("x", "a", "b"), List("y", "modified"), List("z")))
  }
}
