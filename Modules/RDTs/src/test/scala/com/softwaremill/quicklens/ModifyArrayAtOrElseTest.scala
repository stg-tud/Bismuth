package com.softwaremill.deltalens

import com.softwaremill.quicklens.deltalens.*
import com.softwaremill.deltalens.TestData.*
import com.softwaremill.quicklens.deltalens.*

class ModifyArrayAtOrElseTest extends munit.FunSuite {

  test("modify an existing element using atOrElse") {
    assert(deepEquals(modify(ar1)(_.atOrElse(2, A3(A4(A5("default")))).a4.a5.name).using(duplicate), l1at2dup))
  }

  test("append the modified default for a missing index") {
    assert(deepEquals(
      modify(ar1)(_.atOrElse(10, A3(A4(A5("def")))).a4.a5.name).using(duplicate),
      l1 :+ A3(A4(A5("defdef")))
    ))
  }
}
