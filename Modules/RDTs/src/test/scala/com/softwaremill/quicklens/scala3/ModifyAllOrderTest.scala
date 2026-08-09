package com.softwaremill.deltalens.scala3

import com.softwaremill.quicklens.deltalens.*

class ModifyAllOrderTest extends munit.FunSuite {
  import ModifyAllOrderTest.*

  test("apply modifications in the correct order if child is first") {
    val lst: Cons = Cons.fromList(List(1, 2, 3)).get

    val expected: Cons = Cons.fromList(List(1, 2, 4)).get

    val res = lst
      .modifyAll(
        _.tail.each.tail.each,
        _.tail.each
      )
      .using {
        case Cons(head, tail) if head == 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 3 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }

  test("apply modifications in the correct order if parent is first") {
    val lst: Cons = Cons.fromList(List(1, 2, 3)).get

    val expected: Cons = Cons.fromList(List(1, 3, 4)).get

    val res = lst
      .modifyAll(
        _.tail.each,
        _.tail.each.tail.each
      )
      .using {
        case Cons(head, tail) if head == 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 3 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }

  test("apply modifications in the correct order: child, parent, child") {
    val lst: Cons = Cons.fromList(List(1, 2, 3)).get

    val expected: Cons = Cons.fromList(List(1, 3, 5)).get

    val res = lst
      .modifyAll(
        _.tail.each.tail.each,
        _.tail.each,
        _.tail.each.tail.each
      )
      .using {
        case Cons(head, tail) if head >= 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 4 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }

  test("apply modifications in the correct order: child, child, parent") {
    val lst: Cons = Cons.fromList(List(1, 2, 3)).get

    val expected: Cons = Cons.fromList(List(1, 2, 5)).get

    val res = lst
      .modifyAll(
        _.tail.each.tail.each,
        _.tail.each.tail.each,
        _.tail.each
      )
      .using {
        case Cons(head, tail) if head >= 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 4 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }

  test("apply modifications in the correct order on option: child, child, parent") {
    val lst: Option[Cons] = Cons.fromList(List(1, 2, 3))

    val expected: Option[Cons] = Cons.fromList(List(1, 2, 5))

    val res = lst
      .modifyAll(
        _.each.tail.each.tail.each,
        _.each.tail.each.tail.each,
        _.each.tail.each
      )
      .using {
        case Cons(head, tail) if head >= 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 4 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }

  test("apply modifications in the correct order on option: child, parent, child") {
    val lst: Option[Cons] = Cons.fromList(List(1, 2, 3))

    val expected: Option[Cons] = Cons.fromList(List(1, 3, 5))

    val res = lst
      .modifyAll(
        _.each.tail.each.tail.each,
        _.each.tail.each,
        _.each.tail.each.tail.each
      )
      .using {
        case Cons(head, tail) if head >= 3 =>
          Cons(head + 1, tail)
        case Cons(head, Some(Cons(head2, tail))) if head2 == 4 =>
          Cons(head + 1, Some(Cons(head2, tail)))
        case c => c
      }

    assertEquals(res, expected)
  }
}

object ModifyAllOrderTest {
  case class Cons(head: Int, tail: Option[Cons])

  object Cons {
    def fromList(list: List[Int]): Option[Cons] = list match {
      case Nil          => None
      case head :: tail =>
        Some(Cons(head, fromList(tail)))
    }
  }
}
