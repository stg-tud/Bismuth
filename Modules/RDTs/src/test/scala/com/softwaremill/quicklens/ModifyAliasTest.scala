package com.softwaremill.deltalens
import rdts.syntax.deltalens.*

import com.softwaremill.deltalens.ModifyAliasTest.*

object ModifyAliasTest {

  case class State(x: Int)

  type S = State

  sealed trait Expr {
    def i: Int
  }
  case class ListInt(i: Int) extends Expr

  type E = Expr
}

class ModifyAliasTest extends munit.FunSuite {
  test("modify an object declared using type alias") {
    val s: S     = State(0)
    val modified = s.modify(_.x).setTo(1)

    assertEquals(modified.x, 1)
  }

  test("modify a sealed hierarchy declared using type alias") {
    val s: E     = ListInt(0)
    val modified = s.modify(_.i).setTo(1)

    assertEquals(modified.i, 1)
  }
}
