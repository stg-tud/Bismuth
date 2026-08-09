package com.softwaremill.quicklens.scala3

import com.softwaremill.quicklens.*

object LiteralTypeTestData {
  case class Test(f: "foo")
  case class Test1[A](f: A)
}

class LiteralTypeTest extends munit.FunSuite {
  import LiteralTypeTestData.*

  test("modify a literal type field with an explicit parameter") {
    assertEquals(Test("foo").modify["foo"](_.f).setTo("foo"), Test("foo"))
  }

  test("modify a literal type field as a type parameter with an explicit parameter") {
    assert(Test1["foo"]("foo").modify["foo"](_.f).setTo("foo") == Test1("foo"))
  }

  test("not compile for a wrong literal type") {
    assert(compileErrors("""
      import com.softwaremill.quicklens.*
      
      case class Test1[A](f: A)
      
      Test1["foo"]("foo").modify["foo"](_.f).setTo("bar")
    """).nonEmpty)
  }
}
