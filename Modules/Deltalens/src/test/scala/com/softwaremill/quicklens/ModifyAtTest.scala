package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*

class ModifyAtTest extends munit.FunSuite {

  test("modify a non-nested list with case class item") {
    assertEquals(modify(l1)(_.at(2).a4.a5.name).using(duplicate), l1at2dup)
    assertEquals(modify(l1)(_.at(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)), l1at2dup)
  }

  test("modify a nested list using at") {
    assertEquals(modify(ll1)(_.at(2).at(1).name).using(duplicate), ll1at2at1dup)
  }

  test("modify a nested list using at and each") {
    assertEquals(modify(ll1)(_.at(2).each.name).using(duplicate), ll1at2eachdup)
    assertEquals(modify(ll1)(_.each.at(1).name).using(duplicate), ll1eachat1dup)
  }

  test("modify both lists and options") {
    assertEquals(modify(y1)(_.y2.y3.at(1).y4.each.name).using(duplicate), y1at1dup)
  }

  test("throw an exception if there's no element at the given index") {
    intercept[IndexOutOfBoundsException] {
      modify(l1)(_.at(10).a4.a5.name).using(duplicate)
    }
  }
}
