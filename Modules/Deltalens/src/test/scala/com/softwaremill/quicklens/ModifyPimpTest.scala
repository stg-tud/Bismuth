package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData.*

class ModifyPimpTest extends munit.FunSuite {
  test("modify a field once") {
    assertEquals(a1.modify(_.a2.a3.a4.a5.name).using(duplicate), a1dup)
  }

  test("modify a deeply-nested case class field") {
    assertEquals(a1.modify(_.a2.a3.a4.a5.name)
      .using(duplicate)
      .modify(_.a2.a3.a4.a5.name)
      .using(duplicate), a1dupdup)
  }

  test("modify several fields") {
    assertEquals(b1.modifyAll(_.b2, _.b3.each).using(duplicate), b1dupdup)
  }

  test("modify polymorphic case class field") {
    assertEquals(aPoly.modify(_.poly).using(duplicate), aPolyDup)
  }
}
