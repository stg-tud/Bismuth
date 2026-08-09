package com.softwaremill.deltalens.scala3

import rdts.syntax.deltalens.*

class CustomModifyProxyTest extends munit.FunSuite {

  test("correctly modify a class using a custom modify proxy method") {
    case class State(foo: Int)

    inline def set[A](state: State, inline path: State => A, value: A): State =
      modify(state)(path).setTo(value)

    val state    = State(100)
    val res      = set(state, _.foo, 200)
    val expected = State(200)
    assertEquals(res, expected)
  }

}
