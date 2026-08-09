package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData.*

class ModifySeqIndexTest extends munit.FunSuite {

  test("modify a non-nested seq with case class item") {
    assertEquals(modify(s1)(_.index(2).a4.a5.name).using(duplicate), l1at2dup)
    assertEquals(modify(s1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)), l1at2dup)
  }

  test("modify a nested seq using index") {
    assertEquals(modify(ss1)(_.index(2).index(1).name).using(duplicate), ll1at2at1dup)
  }

  test("modify a nested seq using index and each") {
    assertEquals(modify(ss1)(_.index(2).each.name).using(duplicate), ll1at2eachdup)
    assertEquals(modify(ss1)(_.each.index(1).name).using(duplicate), ll1eachat1dup)
  }

  test("not modify if given index does not exist") {
    assertEquals(modify(s1)(_.index(10).a4.a5.name).using(duplicate), l1)
  }
}
