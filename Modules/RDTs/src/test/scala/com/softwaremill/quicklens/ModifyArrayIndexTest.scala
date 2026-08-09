package com.softwaremill.deltalens

import com.softwaremill.quicklens.deltalens.*
import com.softwaremill.deltalens.TestData.*
import com.softwaremill.quicklens.deltalens.*

class ModifyArrayIndexTest extends munit.FunSuite {

  test("modify a non-nested array with case class item") {
    assert(deepEquals(modify(ar1)(_.index(2).a4.a5.name).using(duplicate), l1at2dup))
    assert(deepEquals(
      modify(ar1)(_.index(2))
        .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)),
      l1at2dup
    ))
  }

  test("modify a nested array using index") {
    assert(deepEquals(modify(arar1)(_.index(2).index(1).name).using(duplicate), ll1at2at1dup))
  }

  test("modify a nested array using index and each") {
    assert(deepEquals(modify(arar1)(_.index(2).each.name).using(duplicate), ll1at2eachdup))
    assert(deepEquals(modify(arar1)(_.each.index(1).name).using(duplicate), ll1eachat1dup))
  }

  test("not modify if given index does not exist") {
    assert(deepEquals(modify(ar1)(_.index(10).a4.a5.name).using(duplicate), l1))
  }
}
