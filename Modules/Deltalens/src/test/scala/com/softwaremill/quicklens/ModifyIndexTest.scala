package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData.*

class ModifyIndexTest extends munit.FunSuite {

  test("modify a non-nested list with case class item") {
    assertEquals(modify(l1)(_.index(2).a4.a5.name).using(duplicate), l1at2dup)
    assertEquals(modify(l1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)), l1at2dup)
  }

  test("modify a nested list using index") {
    assertEquals(modify(ll1)(_.index(2).index(1).name).using(duplicate), ll1at2at1dup)
  }

  test("modify a nested list using index and each") {
    assertEquals(modify(ll1)(_.index(2).each.name).using(duplicate), ll1at2eachdup)
    assertEquals(modify(ll1)(_.each.index(1).name).using(duplicate), ll1eachat1dup)
  }

  test("modify both lists and options") {
    assertEquals(modify(y1)(_.y2.y3.index(1).y4.each.name).using(duplicate), y1at1dup)
  }

  test("not modify if given index does not exist") {
    assertEquals(modify(l1)(_.index(10).a4.a5.name).using(duplicate), l1)
  }
}
