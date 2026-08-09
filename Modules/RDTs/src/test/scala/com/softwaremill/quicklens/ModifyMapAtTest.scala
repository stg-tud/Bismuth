package com.softwaremill.deltalens

import com.softwaremill.deltalens.TestData.*
import rdts.syntax.deltalens.*

class ModifyMapAtTest extends munit.FunSuite {

  test("modify a non-nested map with case class item") {
    assertEquals(modify(m1)(_.at("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested map with atOrElse") {
    assertEquals(modify(m1)(_.atOrElse("K1", A4(A5("d4"))).a5.name).using(duplicate), m1dup)
    assertEquals(modify(m1)(_.atOrElse("K1", ???).a5.name).using(duplicate), m1dup)
    assertEquals(modify(m1)(_.atOrElse("K4", A4(A5("d4"))).a5.name).using(duplicate), m1missingdup)
  }

  test("modify a non-nested sorted map with case class item") {
    assertEquals(modify(ms1)(_.at("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested hash map with case class item") {
    assertEquals(modify(mh1)(_.at("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a non-nested listed map with case class item") {
    assertEquals(modify(ml1)(_.at("K1").a5.name).using(duplicate), m1dup)
  }

  test("modify a nested map using at") {
    assertEquals(modify(m2)(_.m3.at("K1").a5.name).using(duplicate), m2dup)
  }

  test("modify a nested map using atOrElse") {
    assertEquals(modify(m2)(_.m3.atOrElse("K4", A4(A5("d4"))).a5.name).using(duplicate), m2missingdup)
  }

  test("modify a non-nested map using each") {
    assertEquals(modify(m1)(_.each.a5.name).using(duplicate), m1dupEach)
  }

  test("modify a non-nested sorted map using each") {
    assertEquals(modify(ms1)(_.each.a5.name).using(duplicate), m1dupEach)
  }

  test("modify a non-nested hash map using each") {
    assertEquals(modify(mh1)(_.each.a5.name).using(duplicate), m1dupEach)
  }

  test("modify a non-nested list map using each") {
    assertEquals(modify(ml1)(_.each.a5.name).using(duplicate), m1dupEach)
  }

  test("throw an exception if there's no such element") {
    intercept[NoSuchElementException] {
      modify(m1)(_.at("K0").a5.name).using(duplicate)
    }
  }

  test("modify a map using at with a derived class") {
    class C
    object D extends C
    val m        = Map[C, String](D -> "")
    val expected = Map(D -> "x")
    assert(modify(m)(_.at(D)).setTo("x") == expected)
  }
}
