package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*
import rdts.syntax.deltalens.*

class ModifyLazyTest extends munit.FunSuite {
  test("modify a single-nested case class field") {
    val ml = modifyLens[A5](_.name).using(duplicate)
    assertEquals(ml(a5), a5dup)
  }

  test("modify a deeply-nested case class field") {
    val ml = modifyLens[A1](_.a2.a3.a4.a5.name).using(duplicate)
    assertEquals(ml(a1), a1dup)
  }

  test("modify several fields") {
    val ml = modifyAllLens[B1](_.b2, _.b3.each).using(duplicate)
    assertEquals(ml(b1), b1dupdup)
  }

  test("modify a case class field if the condition is true") {
    val ml = modifyLens[A5](_.name).usingIf(true)(duplicate)
    assertEquals(ml(a5), a5dup)
  }

  test("leave a case class unchanged if the condition is flase") {
    val ml = modifyLens[A5](_.name).usingIf(false)(duplicate)
    assertEquals(ml(a5), a5)
  }
}
