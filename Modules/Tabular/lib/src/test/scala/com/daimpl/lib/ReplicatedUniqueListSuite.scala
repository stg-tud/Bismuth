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
      val base = withUid("shared initial state") { fromElements("x") }

      var rA = base
      var rB = base

      withUid("A") {
        val deltaA =
          opA match
            case "keep"   => rA.updateAt(0, rA.readAt(0).get)
            case "remove" => rA.removeAt(0)
        rA += deltaA
      }
      withUid("B") {
        val deltaB =
          opB match
            case "keep"   => rB.updateAt(0, rB.readAt(0).get)
            case "remove" => rB.removeAt(0)
        rB += deltaB
      }

      val merged = rA + rB

      assertEqualsList(merged, if shouldBeAlive then List("x") else List())
    }
  }

  test("remove after keep (same replica) erases element") {
    var rA = withUid("A") { fromElements("x") }
    val rB = rA

    withUid("A") {
      rA += rA.updateAt(0, rA.readAt(0).get)
      rA += rA.removeAt(0)
    }

    val merged = rA + rB

    assertEqualsList(merged, List())
  }

  test("concurrent insertAt(0) and insertAt(0) keeps both") {
    var rA = ReplicatedUniqueList.empty[String]
    var rB = ReplicatedUniqueList.empty[String]

    withUid("A") { rA += rA.insertAt(0, "a") }
    withUid("B") { rB += rB.insertAt(0, "b") }

    val merged = rA + rB

    val asList = merged.toList
    assertEquals(asList.toSet, Set("a", "b"))
  }

  test("concurrent prepend and append keeps order") {
    var rA = withUid("A") { fromElements("initial") }
    var rB = rA

    withUid("A") { rA += rA.insertAt(0, "head") }
    withUid("B") { rB += rB.append("tail") }

    val merged = rA + rB

    assertEqualsList(merged, List("head", "initial", "tail"))
  }

  test("insertAt after remote remove keeps consistent index") {
    var rA = withUid("A") { fromElements("x", "y") }
    var rB = rA

    withUid("A") { rA += rA.insertAt(1, "a") }
    withUid("B") { rB += rB.removeAt(0) }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "y"))
  }

  test("move element forward within same replica") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    withUid("A") { rA += rA.move(0, 2) }
    assertEqualsList(rA, List("b", "a", "c"))
  }

  test("move element backward within same replica") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    withUid("A") { rA += rA.move(2, 0) }
    assertEqualsList(rA, List("c", "a", "b"))
  }

  test("add and get marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    val markerId = Uid("marker1")

    withUid("A") { rA += rA.addMarker(markerId, 1) }

    assertEquals(rA.getMarker(markerId), Some(1))
  }

  test("remove marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    val markerId = Uid("marker1")

    withUid("A") {
      rA += rA.addMarker(markerId, 1)
      assertEquals(rA.getMarker(markerId), Some(1))

      rA += rA.removeMarker(markerId)
      assertEquals(rA.getMarker(markerId), None)
    }
  }

  test("concurrent add marker operations") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId1 = Uid("marker1")
    val markerId2 = Uid("marker2")

    withUid("A") { rA += rA.addMarker(markerId1, 0) }
    withUid("B") { rB += rB.addMarker(markerId2, 2) }

    val merged = rA + rB

    assertEquals(merged.getMarker(markerId1), Some(0))
    assertEquals(merged.getMarker(markerId2), Some(2))
  }

  test("marker survives element removal") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId = Uid("marker1")

    withUid("A") {
      rA += rA.addMarker(markerId, 1)
      assertEquals(rA.getMarker(markerId), Some(1))
    }

    withUid("B") {
      rB += rB.removeAt(0)
    }

    val merged = rA + rB

    assertEquals(merged.getMarker(markerId), Some(0))
    assertEqualsList(merged, List("b", "c"))
  }

  test("concurrent add and remove marker") {
    var rA = withUid("A") { fromElements("a", "b", "c") }
    var rB = rA
    val markerId = Uid("marker1")

    withUid("A") { rA += rA.addMarker(markerId, 1) }
    withUid("B") { rB += rB.removeMarker(markerId) }

    val merged = rA + rB

    assertEquals(merged.getMarker(markerId), Some(1))
  }

  test("concurrent (move) and (update): update and move are kept") {
    var rA = withUid("A")(fromElements("a", "b", "c"))
    var rB = rA

    withUid("A") {
      val deltaA = rA.move(1, 3)
      rA += deltaA
    }

    withUid("B") {
      val old = rB.readAt(1).get
      val deltaUpdate = rB.updateAt(1, old + "_updated")
      rB += deltaUpdate
    }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "c", "b_updated"))
  }

  test("concurrent (move, update) and (delete): update wins, move is kept") {
    var rA = withUid("A")(fromElements("a", "b", "c"))
    var rB = rA

    withUid("A") {
      rA += rA.removeAt(1)
    }

    withUid("B") {
      val deltaMove = rB.move(1, 3)
      rB += deltaMove
      val old = rB.readAt(2).get
      val deltaUpdate = rB.updateAt(2, old + "_revived")
      rB += deltaUpdate
    }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "c", "b_revived"))
  }

  test("concurrent (marker insert) and (move, update)") {
    var rA = withUid("A")(fromElements("a", "b", "c"))
    var rB = rA
    val markerId = Uid("markerX")

    withUid("A") {
      rA += rA.addMarker(markerId, 1)
    }

    withUid("B") {
      val deltaMove = rB.move(1, 0)
      rB += deltaMove
      val old = rB.readAt(0).get
      val deltaUpdate = rB.updateAt(0, old + "_updated")
      rB += deltaUpdate
    }

    val merged = rA + rB

    assertEqualsList(merged, List("b_updated", "a", "c"))
    assertEquals(merged.getMarker(markerId), Some(0))
  }

  test("concurrent (move) at range border and (move, update): update and more recent move are kept") {
    val markerId = Uid("b_marker")

    val base =
      withUid("shared initial state") {
        var r = fromElements("a", "b", "c", "d")
        r += r.addMarker(markerId, 1)
        r
      }
    var rA = base
    var rB = base

    withUid("A") {
      val deltaMove = rA.move(1, 3)
      rA += deltaMove
    }

    withUid("B") {
      val deltaMove = rB.move(1, 4)
      rB += deltaMove

      val old = rB.readAt(3).get
      val deltaUpdate = rB.updateAt(3, old + "_updated")
      rB += deltaUpdate
    }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "c", "d", "b_updated"))
    assertEquals(merged.getMarker(markerId), Some(3))
  }

  test("concurrent move and remove 1: move wins, element is retained") {
    val base = withUid("shared initial state")(fromElements("a", "b"))
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(0, 2)
    }
    withUid("B") {
      rB += rB.removeAt(0)
    }

    val merged = rA + rB

    assertEqualsList(merged, List("b", "a"))
  }

  test("concurrent move and remove 2: move wins, element is retained") {
    val base = withUid("shared initial state") {
      fromElements("a", "b", "c")
    }
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(1, 3)
    }
    withUid("B") {
      rB += rB.removeAt(1)
    }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "c", "b"))
  }

  test("concurrent deletion and edit: marker is kept and not moved") {
    val markerId = Uid("marker")
    val markerPos = 1

    val base =
      withUid("shared initial state") {
        var r = fromElements("a", "b", "c")
        r += r.addMarker(markerId, markerPos)
        r
      }
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.removeAt(markerPos)
    }
    withUid("B") {
      rB += rB.updateAt(markerPos, "b_updated")
    }

    val merged = rA + rB

    assertEqualsList(merged, List("a", "b_updated", "c"))
    assertEquals(merged.getMarker(markerId), Some(markerPos))
  }

  test("concurrent move and update: move wins, element is retained") {
    val base = withUid("shared initial state")(fromElements("a", "b"))
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(0, 2)
    }
    withUid("B") {
      rB += rB.updateAt(0, "a_updated")
    }

    val merged = rA + rB
    assertEqualsList(merged, List("b", "a_updated"))
  }

  test("concurrent update and delete: update wins") {
    val base = withUid("shared initial state") {
      fromElements("r1", "r2", "r3")
    }
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.updateAt(1, "r2_edited")
    }
    withUid("B") {
      rB += rB.removeAt(1)
    }

    val merged = rA + rB

    assertEquals(merged.readAt(1), Some("r2_edited"))
  }

  test("marker follows move of its element") {
    var rA = withUid("A")(fromElements("a", "b", "c"))
    val markerId = Uid("m1")

    withUid("A") {
      rA += rA.addMarker(markerId, 0)
      rA += rA.move(0, 2)
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
        rA += rA.addMarker(id, 1, behaviour)
      }
      withUid("A") {
        rA += rA.removeAt(1)
      }

      assertEquals(rA.getMarker(id), expectedIdx)
    }
  }

  test("move vs move of same element converges") {
    val base = withUid("shared initial state")(fromElements("a", "b", "c"))
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(0, 2)
    }
    withUid("B") {
      rB += rB.move(0, 1)
    }

    val merged = rA + rB
    assertEquals(merged.toList.toSet, Set("a", "b", "c"))
  }

  test("crossing moves") {
    val base = withUid("shared initial state")(fromElements("a", "b", "c", "d"))
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(0, 3)
    }
    withUid("B") {
      rB += rB.move(3, 0)
    }

    val merged = rA + rB
    assertEquals(merged.toList.toSet, Set("a", "b", "c", "d"))
  }

  test("crossing moves break cycles deterministically: later move wins") {
    val base = withUid("shared initial state") {
      fromElements("a", "b")
    }
    var r1 = base
    var r2 = base

    withUid("A") {
      r1 = r1 + r1.move(0, 2)
    }
    withUid("B") {
      r2 = r2 + r2.move(1, 0)
    }

    val merged = r1 + r2

    assertEquals(merged.size, 2)
    assertEqualsList(merged, List("b", "a"))
  }

  test("concurrent move and insert at destination slot") {
    val base = withUid("shared initial state")(fromElements("a", "b"))
    var rA = base
    var rB = base

    withUid("A") {
      rA += rA.move(0, 2)
    }
    withUid("B") {
      rB += rB.insertAt(2, "c")
    }

    val merged = rA + rB
    assertEqualsList(merged, List("b", "c", "a"))
  }

  test("associative merge across three replicas") {
    val base = withUid("shared initial state") {
      fromElements("a", "b")
    }
    var rA = base
    var rB = base
    var rC = base

    withUid("A") {
      rA += rA.insertAt(1, "c")
    }
    withUid("B") {
      rB += rB.removeAt(0)
    }
    withUid("C") {
      rC += rC.move(1, 2)
    }

    val ab_c = (rA + rB) + rC
    val a_bc = rA + (rB + rC)
    assertEqualsList(ab_c, a_bc.toList)
  }

  test("concurrent moves of same element do not duplicate: later move wins") {
    val base = withUid("shared initial state") {
      fromElements("a", "b", "c")
    }
    var r1 = base
    var r2 = base

    withUid("A") {
      r1 = r1 + r1.move(1, 0)
    }
    withUid("B") {
      r2 = r2 + r2.move(1, 3)
    }

    val merged = r1 + r2
    assertEqualsList(merged, List("a", "c", "b"))
  }

  test("two concurrent inserts at same index") {
    var r1 = ReplicatedUniqueList.empty[String]
    var r2 = r1

    withUid("A") {
      r1 = r1 + r1.insertAt(0, "insert1")
    }
    withUid("B") {
      r2 = r2 + r2.insertAt(0, "insert2")
    }

    val merged = r1 + r2
    assertEquals(merged.toList, List("insert2", "insert1"))
  }

  test("concurrent (insert) and (delete) adjust indices correctly") {
    val base = withUid("shared initial state") {
      fromElements("a", "b", "c")
    }
    var r1 = base
    var r2 = base

    withUid("A") {
      r1 = r1 + r1.insertAt(1, "new")
    }
    withUid("B") {
      r2 = r2 + r2.removeAt(0)
    }

    val merged = r1 + r2
    assertEqualsList(merged, List("new", "b", "c"))
  }

  test("many concurrent operations converge to a valid state") {
    val base = withUid("shared initial state") {
      fromElements((1 to 5).map(_.toString)*)
    }
    var rA = base
    var rB = base
    var rC = base

    withUid("A") {
      rA += rA.insertAt(2, "a")
      rA += rA.move(0, 4)
    }
    withUid("B") {
      rB += rB.removeAt(3)
      rB += rB.insertAt(1, "b")
      rB += rB.updateAt(0, "a_updated")
    }
    withUid("C") {
      rC += rC.move(4, 1)
      rC += rC.removeAt(0)
    }

    val merged = rA + rB + rC

    assertEquals(merged.toList.size, merged.size)
  }

  private def fromElements[E](elems: E*)(using uid: LocalUid): ReplicatedUniqueList[E] =
    elems.foldLeft(ReplicatedUniqueList.empty[E]) { (state, e) => state + state.append(e) }

  private def assertEqualsList[E](actual: ReplicatedUniqueList[E], expected: List[E]): Unit =
    assertEquals(actual.toList, expected)
    assertEquals(actual.size, expected.size)

    expected.zipWithIndex.foreach { case (elem, idx) =>
      assertEquals(actual.readAt(idx), Some(elem))
    }

    assertEquals(actual.readAt(expected.size), None)
    assertEquals(actual.readAt(expected.size + 1), None)
