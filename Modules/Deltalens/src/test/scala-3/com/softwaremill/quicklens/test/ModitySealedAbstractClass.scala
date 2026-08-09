package com.softwaremill.quicklens.test

import com.softwaremill.quicklens.*
import com.softwaremill.quicklens.TestData.*

class ModitySealedAbstractClass extends munit.FunSuite {
  test("Modify abstract class hierarchy") {
    assertEquals(invInt.modify(_.typ).setTo(Type("Long")), invLong)
  }
}
