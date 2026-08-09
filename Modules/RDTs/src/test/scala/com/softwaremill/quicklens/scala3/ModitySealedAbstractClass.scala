package com.softwaremill.deltalens.scala3

import com.softwaremill.quicklens.deltalens.*
import com.softwaremill.deltalens.TestData.*
import com.softwaremill.quicklens.deltalens.*

class ModitySealedAbstractClass extends munit.FunSuite {
  test("Modify abstract class hierarchy") {
    assertEquals(invInt.modify(_.typ).setTo(Type("Long")), invLong)
  }
}
