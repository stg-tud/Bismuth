package com.softwaremill.deltalens

import com.softwaremill.quicklens.deltalens.*
import com.softwaremill.deltalens.TestData.*
import com.softwaremill.quicklens.deltalens.*

class ModifySimpleTest extends munit.FunSuite {
  test("modify a single-nested case class field") {
    assertEquals(modify(a5)(_.name).using(duplicate), a5dup)
  }

  test("modify a single-nested case class field using apply") {
    assertEquals(modify(a5)(_.name)(duplicate), a5dup)
  }

  test("modify a deeply-nested case class field") {
    assertEquals(modify(a1)(_.a2.a3.a4.a5.name).using(duplicate), a1dup)
  }

  test("modify several fields") {
    assertEquals(modifyAll(b1)(_.b2, _.b3.each).using(duplicate), b1dupdup)
  }

  test("modify a case class field if the condition is true") {
    assertEquals(modify(a5)(_.name).usingIf(true)(duplicate), a5dup)
  }

  test("leave a case class unchanged if the condition is false") {
    assertEquals(modify(a5)(_.name).usingIf(false)(duplicate), a5)
  }

  test("modify polymorphic case class field") {
    assertEquals(modify(aPoly)(_.poly).using(duplicate), aPolyDup)
  }

  test("modify polymorphic case class field using apply") {
    assertEquals(modify(aPoly)(_.poly)(duplicate), aPolyDup)
  }

  test("modify polymorphic case class field if condition is true") {
    assertEquals(modify(aPoly)(_.poly).usingIf(true)(duplicate), aPolyDup)
  }
  test("leave a polymorphic case class field if condition is false") {
    assertEquals(modify(aPoly)(_.poly).usingIf(false)(duplicate), aPoly)
  }
}
