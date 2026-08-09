package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*
import rdts.syntax.deltalens.*

class SetToSimpleTest extends munit.FunSuite {
  test("set a new value of a single-nested case class field") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).setTo("mod"), a1mod)
  }

  test("set a new value in a case class if the condition is true") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).setToIf(true)("mod"), a1mod)
  }

  test("leave a case class unchanged if the condition is false") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).setToIf(false)("mod"), a1)
  }

  test("set a new value in a case class if it is defined") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).setToIfDefined(Some("mod")), a1mod)
  }

  test("leave a case class unchanged if the value is not defined") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).setToIfDefined(None), a1)
  }
}
