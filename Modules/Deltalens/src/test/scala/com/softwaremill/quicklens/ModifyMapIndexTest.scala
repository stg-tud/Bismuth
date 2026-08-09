package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*

class ModifyMapIndexTest extends munit.FunSuite {

  test("modify a non-nested map with case class item") {
    assertEquals(modify(m1)(_.index("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested sorted map with case class item") {
    assertEquals(modify(ms1)(_.index("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested hash map with case class item") {
    assertEquals(modify(mh1)(_.index("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested listed map with case class item") {
    assertEquals(modify(ml1)(_.index("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a nested map using index") {
    assertEquals(modify(m2)(_.m3.index("K1").a5.name).using(duplicate), m2dup)
  }

  test("not modify if there's no such element") {
    assertEquals(modify(m1)(_.index("K0").a5.name).using(duplicate), m1)
  }
}
