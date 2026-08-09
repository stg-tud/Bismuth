package com.softwaremill.deltalens

import rdts.syntax.deltalens.*

class TupleModifyTest extends munit.FunSuite {
  test("modify case classes using setTo") {
    case class Pair(a: Int, b: Int)
    val p        = Pair(0, 1)
    val modified = p.modify(_.b).setTo(2)
    assertEquals(modified, Pair(0, 2))
  }
  test("modify tuples using setTo") {
    val tuple    = (0, 1)
    val modified = tuple.modify(_._2).setTo(2)
    assertEquals(modified, (0, 2))
  }
  test("modify tuples using using") {
    val tuple4   = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1)
    assertEquals(modified, (0, 1, 3, 3))
  }

  test("modify tuples using multiple modify") {
    val tuple4   = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1).modify(_._4).using(_ + 1)
    assertEquals(modified, (0, 1, 3, 4))
  }
}
