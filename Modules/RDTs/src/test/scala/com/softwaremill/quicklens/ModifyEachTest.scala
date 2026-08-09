package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*

class ModifyEachTest extends munit.FunSuite {
  test("modify a single-nested optional case class field") {
    assertEquals(modify(x4)(_.x5.each.name).using(duplicate), x4dup)
  }

  test("modify a single-nested optional case class field (pimped)") {
    assertEquals(x4.modify(_.x5.each.name).using(duplicate), x4dup)
  }

  test("modify multiple deeply-nested optional case class field") {
    assertEquals(modify(x1)(_.x2.x3.each.x4.x5.each.name).using(duplicate), x1dup)
  }

  test("not modify an optional case class field if it is none") {
    assertEquals(modify(x1none)(_.x2.x3.each.x4.x5.each.name).using(duplicate), x1none)
    assertEquals(modify(x4none)(_.x5.each.name).using(duplicate), x4none)
  }

  test("modify both lists and options") {
    assertEquals(modify(y1)(_.y2.y3.each.y4.each.name).using(duplicate), y1dup)
  }

  test("allow .each at the end") {
    assertEquals(modify(z1)(_.name.each).using(duplicate), z1dup)
  }
}
