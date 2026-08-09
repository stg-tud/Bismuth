package com.softwaremill.deltalens


class ModifyOptionAtOrElseTest extends munit.FunSuite {

  test("modify a Some") {
    assertEquals(modify(Option(1))(_.atOrElse(3)).using(_ + 1), Option(2))
  }

  test("modify a None with default") {
    assertEquals(modify(None: Option[Int])(_.atOrElse(3)).using(_ + 1), Option(4))
  }

  test("modify a Option in a case class hierarchy") {
    case class Foo(a: Int)
    case class Bar(foo: Foo)
    case class BarOpt(maybeBar: Option[Bar])
    case class BazOpt(barOpt: BarOpt)
    assertEquals(modify(BazOpt(BarOpt(None)))(_.barOpt.maybeBar.atOrElse(Bar(Foo(5))).foo.a).using(_ + 1),
      BazOpt(BarOpt(Some(Bar(Foo(6)))))
    )
  }
}
