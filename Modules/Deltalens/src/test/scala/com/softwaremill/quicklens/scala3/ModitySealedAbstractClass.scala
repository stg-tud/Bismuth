package com.softwaremill.deltalens.scala3

import com.softwaremill.deltalens.*
import com.softwaremill.deltalens.TestData.*

class ModitySealedAbstractClass extends munit.FunSuite {
  test("Modify abstract class hierarchy") {
    assertEquals(invInt.modify(_.typ).setTo(Type("Long")), invLong)
  }
}
