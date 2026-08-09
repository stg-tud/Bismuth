package com.softwaremill.deltalens

import scala.annotation.nowarn
import rdts.syntax.deltalens.*

class SecondParamListTest extends munit.FunSuite {
  test("modify an object with second implicit param list") {

    @nowarn("id=E198")
    case class State(inside: Boolean)(implicit d: Double)

    val d: Double = 1.0

    val state1 = State(true)(using d)

    given dd: Double = d
    val state2       = state1.modify(_.inside).setTo(true)

    assertEquals(state1, state2)
  }

  test("should give a meaningful error for an object with more than one non-implicit param list") {

    case class State(inside: Boolean)(d: Double)

    val d: Double = 1.0

    val state1 = State(true)(d)

    given dd: Double = d

    assert(compileErrors("state1.modify(_.inside).setTo(true)").nonEmpty)
  }
}
