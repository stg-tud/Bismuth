package com.softwaremill.deltalens

import com.softwaremill.quicklens.deltalens.*

case class Named(name: String)

case class Aged(age: Int)

case class Eithers(e: Either[Named, Aged])

case class MoreEithers(e1: Either[Eithers, MoreEithers], e2: Either[Eithers, MoreEithers])

class ModifyEitherTest extends munit.FunSuite {

  test("modify a single-nested left case class field") {
    assertEquals(
      modify(
        Eithers(Left(Named("boo")))
      )(_.e.eachLeft.name).setTo("moo"),
      Eithers(Left(Named("moo")))
    )
  }

  test("modify a single-nested left case class field (pimped)") {
    assertEquals(
      Eithers(Left(Named("boo")))
        .modify(_.e.eachLeft.name)
        .setTo("moo"),
      Eithers(Left(Named("moo")))
    )
  }

  test("modify a single-nested right case class field") {
    assertEquals(
      modify(
        Eithers(Right(Aged(23)))
      )(_.e.eachRight.age).setTo(32),
      Eithers(Right(Aged(32)))
    )
  }

  test("modify a single-nested right case class field (pimped)") {
    assertEquals(
      Eithers(Right(Aged(23)))
        .modify(_.e.eachRight.age)
        .setTo(32),
      Eithers(Right(Aged(32)))
    )
  }

  test("modify multiple deeply-nested either case class fields") {

    assertEquals(
      modify(
        MoreEithers(
          e1 = Right(
            MoreEithers(
              e1 = Left(Eithers(Right(Aged(23)))),
              e2 = Left(Eithers(Left(Named("boo"))))
            )
          ),
          e2 = Left(Eithers(Left(Named("boo"))))
        )
      )(_.e1.eachRight.e2.eachLeft.e.eachLeft.name)
        .using(_.toUpperCase),
      MoreEithers(
        e1 = Right(
          MoreEithers(
            e1 = Left(Eithers(Right(Aged(23)))),
            e2 = Left(Eithers(Left(Named("BOO"))))
          )
        ),
        e2 = Left(Eithers(Left(Named("boo"))))
      )
    )
  }

  test("not modify left case class field if it is right") {
    assertEquals(
      modify(
        Eithers(Right(Aged(23)))
      )(_.e.eachLeft.name).setTo("moo"),
      Eithers(Right(Aged(23)))
    )
  }

  test("not modify right case class field if it is left") {
    assertEquals(
      modify(
        Eithers(Left(Named("boo")))
      )(_.e.eachRight.age).setTo(33),
      Eithers(Left(Named("boo")))
    )
  }

  test("allow .eachLeft at then end") {
    assertEquals(
      modify(Left("boo"): Either[String, Int])(_.eachLeft)
        .using(_.toUpperCase),
      Left("BOO")
    )
  }

  test("allow .eachRight at then end") {
    assertEquals(modify(Right(23): Either[String, Int])(_.eachRight).using(_ + 3), Right(26))
  }
}
