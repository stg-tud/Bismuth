package com.softwaremill.quicklens


class ModifySeqAtOrElseTest extends munit.FunSuite {

  case class Item(name: String)

  test("modify an existing element using atOrElse") {
    val items = List(Item("a"), Item("b"))
    assertEquals(modify(items)(_.atOrElse(1, Item("default")).name).using(_.toUpperCase),
      List(Item("a"), Item("B"))
    )
  }

  test("use the default for a missing index instead of throwing") {
    val items = List(Item("a"), Item("b"))
    assertEquals(modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase),
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }

  test("append the modified default on an empty sequence") {
    val items = List.empty[Item]
    assertEquals(modify(items)(_.atOrElse(0, Item("default")).name).using(_.toUpperCase),
      List(Item("DEFAULT"))
    )
  }

  test("append the modified default for a negative index") {
    val items = List(Item("a"), Item("b"))
    assertEquals(modify(items)(_.atOrElse(-1, Item("default")).name).using(_.toUpperCase),
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }
}
