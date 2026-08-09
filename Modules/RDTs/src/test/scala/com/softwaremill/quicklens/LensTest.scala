package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*
import rdts.syntax.deltalens.*

class LensTest extends munit.FunSuite {
  test("create reusable lens of the given type") {
    val lens = modify(_: A1)(_.a2.a3.a4.a5.name)

    assertEquals(lens(a1).using(duplicate), a1dup)
  }

  test("compose lens") {
    val lens_a1_a3   = modify(_: A1)(_.a2.a3)
    val lens_a3_name = modify(_: A3)(_.a4.a5.name)

    assertEquals(lens_a1_a3.andThenModify(lens_a3_name)(a1).using(duplicate), a1dup)
  }
}
