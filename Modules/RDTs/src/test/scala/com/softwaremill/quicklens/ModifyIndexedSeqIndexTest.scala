package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*
import com.softwaremill.quicklens.deltalens.*
import com.softwaremill.quicklens.deltalens.*

class ModifyIndexedSeqIndexTest extends munit.FunSuite {

  test("modify a non-nested indexed seq with case class item") {
    assert(modify(is1)(_.index(2).a4.a5.name).using(duplicate) == l1at2dup)
    assert(modify(is1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) == l1at2dup)
  }

  test("modify a nested indexed seq using index") {
    assert(modify(iss1)(_.index(2).index(1).name).using(duplicate) == ll1at2at1dup)
  }

  test("modify a nested indexed seq using index and each") {
    assert(modify(iss1)(_.index(2).each.name).using(duplicate) == ll1at2eachdup)
    assert(modify(iss1)(_.each.index(1).name).using(duplicate) == ll1eachat1dup)
  }

  test("not modify if given index does not exist") {
    assert(modify(is1)(_.index(10).a4.a5.name).using(duplicate) == l1)
  }
}
