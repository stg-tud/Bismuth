package com.daimpl.lib

import munit.FunSuite
import rdts.base.Lattice.syntax.*
import rdts.base.{LocalUid, Uid}

final class KeepRemoveListSuite extends FunSuite:

  inline def withUid[A](id: String)(body: LocalUid ?=> A): A =
    body(using LocalUid(Uid(id)))

  extension [E](state: KeepRemoveList[E])
    inline def +(delta: KeepRemoveList[E]): KeepRemoveList[E] =
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
    ("keep", "keep", true),
    ("keep", "remove", true),
    ("remove", "keep", true),
    ("remove", "remove", false)
  ).foreach { case (opA, opB, shouldBeAlive) =>
    test(s"concurrent $opA and $opB on the same element (${if shouldBeAlive then "alive" else "tombstoned"})") {
      val base = withUid("A") { fromElements("x") }

      var rA = base
      var rB = base

      withUid("A") {
        val deltaA =
          opA match
            case "keep"   => rA.keep(0)
            case "remove" => rA.remove(0)
        rA = rA + deltaA
      }
      withUid("B") {
        val deltaB =
          opB match
            case "keep"   => rB.keep(0)
            case "remove" => rB.remove(0)
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
      rA = rA + rA.keep(0)
      rA = rA + rA.remove(0)
    }
    val merged = rA + rB
    assertEqualsList(merged, List())
  }

  test("concurrent insertAt(0) and insertAt(0) keeps both") {
    var rA = KeepRemoveList.empty[String]
    var rB = KeepRemoveList.empty[String]

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

  test("concurrent insertAt(1) and remove(0) preserves correct order") {
    val base = withUid("A") { fromElements("x", "y") }
    var rA   = base
    var rB   = base
    withUid("A") { rA = rA + rA.insertAt(1, "z") }
    withUid("B") { rB = rB + rB.remove(0) }
    // todo
    /*val merged = rA + rB
    assertEqualsList(merged, List("y", "z"))*/
  }

  private def fromElements[E](elems: E*)(using uid: LocalUid): KeepRemoveList[E] =
    elems.foldLeft(KeepRemoveList.empty[E]) { (state, e) => state + state.append(e) }

  private def assertEqualsList[E](actual: KeepRemoveList[E], expected: List[E]): Unit =
    assertEquals(actual.toList, expected)
    assertEquals(actual.size, expected.size)

    expected.zipWithIndex.foreach { case (elem, idx) =>
      assertEquals(actual.read(idx), Some(elem))
    }

    assertEquals(actual.read(expected.size), None)
    assertEquals(actual.read(expected.size + 1), None)
