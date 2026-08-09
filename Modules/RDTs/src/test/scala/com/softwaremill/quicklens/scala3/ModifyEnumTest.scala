package com.softwaremill.deltalens.scala3

import rdts.syntax.deltalens.*
import com.softwaremill.deltalens.TestData.duplicate

object EnumTestData {
  enum P3(val a: String):
      case C5(override val a: String, b: Int)            extends P3(a)
      case C6(override val a: String, c: Option[String]) extends P3(a)

  val p3: P3    = P3.C5("c2", 0)
  val p3dup: P3 = P3.C5("c2c2", 0)
}

class ModifyEnumTest extends munit.FunSuite {
  import EnumTestData.*

  test("modify a field in an enum case") {
    assertEquals(modify(p3)(_.a).using(duplicate), p3dup)
  }

  test("modify a field in an enum case with extension method") {
    assertEquals(p3.modify(_.a).using(duplicate), p3dup)
  }
}
