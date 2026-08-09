package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData.*

class LensLazyTest extends munit.FunSuite {
  test("create reusable lens of the given type") {
    val lens = modifyLens[A1](_.a2.a3.a4.a5.name)

    val lm = lens.using(duplicate)
    assertEquals(lm(a1), a1dup)
  }

  test("compose lens") {
    val lens_a1_a3   = modifyLens[A1](_.a2.a3)
    val lens_a3_name = modifyLens[A3](_.a4.a5.name)

    val lm = lens_a1_a3.andThenModify(lens_a3_name).using(duplicate)
    assertEquals(lm(a1), a1dup)
  }
}
