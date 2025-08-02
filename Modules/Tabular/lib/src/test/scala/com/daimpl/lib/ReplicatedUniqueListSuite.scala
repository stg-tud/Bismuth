package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.MarkerRemovalBehavior
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
            case "remove" => rA.removeAt(0)
        rA = rA + deltaA
      }
      withUid("B") {
        val deltaB =
          opB match
            case "keep"   => rB.insertAt(0, rB.read(0).get)
            case "remove" => rB.removeAt(0)
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
      rA = rA + rA.removeAt(0)
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
    withUid("B") { rB = rB + rB.removeAt(0) }

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

  test("add and get marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    val markerId = Uid("marker1")

    withUid("A") { rA = rA + rA.addMarker(markerId, 1) }

    assertEquals(rA.getMarker(markerId), Some(1))
  }

  test("remove marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    val markerId = Uid("marker1")

    withUid("A") {
      rA = rA + rA.addMarker(markerId, 1)
      assertEquals(rA.getMarker(markerId), Some(1))

      rA = rA + rA.removeMarker(markerId)
      assertEquals(rA.getMarker(markerId), None)
    }
  }

  test("concurrent add marker operations") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId1 = Uid("marker1")
    val markerId2 = Uid("marker2")

    withUid("A") { rA = rA + rA.addMarker(markerId1, 0) }
    withUid("B") { rB = rB + rB.addMarker(markerId2, 2) }

    val merged = rA + rB

    assertEquals(merged.getMarker(markerId1), Some(0))
    assertEquals(merged.getMarker(markerId2), Some(2))
  }

  test("marker survives element removal") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId = Uid("marker1")

    withUid("A") {
      rA = rA + rA.addMarker(markerId, 1)
      assertEquals(rA.getMarker(markerId), Some(1))
    }

    withUid("B") {
      rB = rB + rB.removeAt(0)
    }

    val merged = rA + rB

    assertEquals(merged.getMarker(markerId), Some(0))
    assertEqualsList(merged, List("b", "c"))
  }

  test("concurrent add and remove marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId = Uid("marker1")

    withUid("A") { rA = rA + rA.addMarker(markerId, 1) }
    withUid("B") { rB = rB + rB.removeMarker(markerId) }

    val merged = rA + rB
    assertEquals(merged.getMarker(markerId), Some(1))
  }

  test("concurrent move and remove") {
    val base = withUid("A")(fromElements("a", "b"))
    var rA = base
    var rB = base

    withUid("A") {
      rA = rA + rA.move(0, 2)
    }
    withUid("B") {
      rB = rB + rB.removeAt(0)
    }

    val merged = rA + rB
    assertEqualsList(merged, List("b", "a"))
  }

  test("concurrent move and update") {
    val base = withUid("A")(fromElements("a", "b"))
    var rA = base
    var rB = base

    withUid("A") {
      rA = rA + rA.move(0, 2)
    }
    withUid("B") {
      rB = rB + rB.update(0, "A")
    }

    val merged = rA + rB
    assertEqualsList(merged, List("A", "b", "a"))
  }

  test("marker follows move of its element") {
    var rA = withUid("A")(fromElements("a", "b", "c"))
    val markerId = Uid("m1")

    withUid("A") {
      rA = rA + rA.addMarker(markerId, 0)
      rA = rA + rA.move(0, 2)
    }

    assertEquals(rA.getMarker(markerId), Some(1))
    assertEqualsList(rA, List("b", "a", "c"))
  }

  Seq(
    (MarkerRemovalBehavior.Predecessor, Some(0), "moves to predecessor"),
    (MarkerRemovalBehavior.Successor, Some(1), "moves to successor"),
    (MarkerRemovalBehavior.None, None, "is removed")
  ).foreach { case (behaviour, expectedIdx, title) =>
    test(s"marker with behaviour $behaviour $title when element is removed") {
      var rA = withUid("A")(fromElements("a", "b", "c"))
      val id = Uid("m-$behaviour")

      withUid("A") {
        rA = rA + rA.addMarker(id, 1, behaviour)
      }
      withUid("A") {
        rA = rA + rA.removeAt(1)
      }

      assertEquals(rA.getMarker(id), expectedIdx)
    }
  }

  test("move vs move of same element converges") {
    val base = withUid("A")(fromElements("a", "b", "c"))
    var rA = base
    var rB = base

    withUid("A") {
      rA = rA + rA.move(0, 2)
    }
    withUid("B") {
      rB = rB + rB.move(0, 1)
    }

    val merged = rA + rB
    assertEquals(merged.toList.toSet, Set("a", "b", "c"))
  }

  test("crossing moves") {
    val base = withUid("A")(fromElements("a", "b", "c", "d"))
    var rA = base;
    var rB = base

    withUid("A") {
      rA = rA + rA.move(0, 3)
    }
    withUid("B") {
      rB = rB + rB.move(3, 0)
    }

    val merged = rA + rB
    assertEquals(merged.toList.toSet, Set("a", "b", "c", "d"))
  }

  test("concurrent move and insert at destination slot") {
    val base = withUid("A")(fromElements("x", "y"))
    var rA = base;
    var rB = base

    withUid("A") {
      rA = rA + rA.move(0, 2)
    }
    withUid("B") {
      rB = rB + rB.insertAt(2, "z")
    }

    val merged = rA + rB
    assertEqualsList(merged, List("y", "z", "x"))
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
