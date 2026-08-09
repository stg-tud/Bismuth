package com.softwaremill.deltalens

import com.softwaremill.deltalens.ModifySelfThisTest.*

object ModifySelfThisTest {

  case class State(x: Int) { self =>

    def mod: State = this.modify(_.x).setTo(1)
  }

  trait A {
    def a: Unit
  }

  case class State1(x: Int) extends A { self: A =>

    def mod: State1 = this.modify(_.x).setTo(1)

    def a: Unit = ()
  }
}

class ModifySelfThisTest extends munit.FunSuite {
  test("modify an object even in presence of self alias") {
    val s        = State(0)
    val modified = s.mod

    assertEquals(modified.x, 1)
  }

  test("modify an object even in presence of self type") {
    val s        = State(0)
    val modified = s.mod

    assertEquals(modified.x, 1)
  }
}
