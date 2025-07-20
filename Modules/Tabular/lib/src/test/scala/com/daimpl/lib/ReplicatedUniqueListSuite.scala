package com.daimpl.lib

import munit.FunSuite
import rdts.base.Lattice.syntax.*
import rdts.base.{LocalUid, Uid}

final class ReplicatedUniqueListSuite extends FunSuite:

  inline def withUid[A](id: String)(body: LocalUid ?=> A): A =
    body(using LocalUid(Uid(id)))

  extension [E](state: ReplicatedUniqueList[E])
    inline def +(delta: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
      val ab = state.merge(delta)
      val ba = delta.merge(state)
      assertEquals(
        obtained = ab,
        expected = ba,
        clue = "merge is not commutative"
      )
      assertEquals(
        obtained = ab.merge(ab),
        expected = ab,
        clue = "merge is not idempotent"
      )
      ab

  Seq(
    ("keep"   , "keep"   , true ),
    ("keep"   , "remove" , true ),
    ("remove" , "keep"   , true ),
    ("remove" , "remove" , false)
  ).foreach { case (opA, opB, shouldBeAlive) =>
    test(s"concurrent $opA and $opB on the same element (${if shouldBeAlive then "alive" else "tombstoned"})") {
      val base = withUid("A") { fromElements("x") }

      var rA = base
      var rB = base

      withUid("A") {
        val deltaA =
          opA match
            case "keep"   => rA.insertAt(0, rA.read(0).get)
            case "remove" => rA.removeIndex(0)
        rA = rA + deltaA
      }
      withUid("B") {
        val deltaB =
          opB match
            case "keep"   => rB.insertAt(0, rB.read(0).get)
            case "remove" => rB.removeIndex(0)
        rB = rB + deltaB
      }

      val merged = rA + rB
      assertEqualsList(merged, if shouldBeAlive then List("x") else List())
    }
  }

  test("remove after keep (same replica) erases element") {
    var rA = withUid("A") { fromElements("x") }
    val rB = rA
    withUid("A") {
      rA = rA + rA.insertAt(0, rA.read(0).get)
      rA = rA + rA.removeIndex(0)
    }
    val merged = rA + rB
    assertEqualsList(merged, List())
  }

  test("concurrent insertAt(0) and insertAt(0) keeps both") {
    var rA = ReplicatedUniqueList.empty[String]
    var rB = ReplicatedUniqueList.empty[String]

    withUid("A") { rA = rA + rA.insertAt(0, "a") }
    withUid("B") { rB = rB + rB.insertAt(0, "b") }

    val merged = rA + rB

    val asList = merged.toList
    assertEquals(asList.toSet, Set("a", "b"))
  }

  test("concurrent prepend and append keeps order") {
    var rA = withUid("A") { fromElements("initial") }
    var rB = rA

    withUid("A") { rA = rA + rA.insertAt(0, "head") }
    withUid("B") { rB = rB + rB.append("tail") }

    val merged = rA + rB
    assertEqualsList(merged, List("head", "initial", "tail"))
  }

  test("insertAt after remote remove keeps consistent index") {
    var rA = withUid("A") { fromElements("x", "y") }
    var rB = rA

    withUid("A") { rA = rA + rA.insertAt(1, "a") }
    withUid("B") { rB = rB + rB.removeIndex(0) }

    val merged = rA + rB
    assertEqualsList(merged, List("a", "y"))
  }

  test("move element forward within same replica") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    withUid("A") { rA = rA + rA.move(0, 2) }
    assertEqualsList(rA, List("b", "a", "c"))
  }

  test("move element backward within same replica") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    withUid("A") { rA = rA + rA.move(2, 0) }
    assertEqualsList(rA, List("c", "a", "b"))
  }

  private def fromElements[E](elems: E*)(using uid: LocalUid): ReplicatedUniqueList[E] =
    elems.foldLeft(ReplicatedUniqueList.empty[E]) { (state, e) => state + state.append(e) }

  private def assertEqualsList[E](actual: ReplicatedUniqueList[E], expected: List[E]): Unit =
    assertEquals(actual.toList, expected)
    assertEquals(actual.size, expected.size)

    expected.zipWithIndex.foreach { case (elem, idx) =>
      assertEquals(actual.read(idx), Some(elem))
    }

    assertEquals(actual.read(expected.size), None)
    assertEquals(actual.read(expected.size + 1), None)
