package test.rdts.bespoke
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.{GrowOnlyList, ReplicatedList}
import rdts.time.Dot

import scala.language.implicitConversions

class ReplicatedListTest extends munit.FunSuite {

  test("insert into grow only list") {
    val v0 = GrowOnlyList.empty[String]
    val v1 = v0.insertGL(0, "00")

    assertEquals(v1.toList, List("00"))

    val vr = v0.insertGL(0, "0r")
    assertEquals(vr.toList, List("0r"))

    val vmerged = v1 `merge` vr
    assertEquals(vmerged.toList, List("0r", "00"))

    val v2 = v1 `merge` v1.insertGL(1, "01")

    assertEquals(v2.toList, List("00", "01"))

    val v3 = v2 `merge` v2.insertGL(0, "02")
    val v4 = v3 `merge` v3.insertGL(1, "10")
    assertEquals(v4.toList, List("02", "10", "00", "01"))

    val recomposed = v4.decomposed.foldLeft(GrowOnlyList.empty[String])(_ `merge` _)
    assertEquals(v4, recomposed)
  }

  test("insert into replicated list") {
    val v1 = ReplicatedList.empty[String]

    val aid = Uid.predefined("a")
    val bid = Uid.predefined("a")

    val v2 = v1.insert(using aid)(0, "10")

    assertEquals(v2.toList, List("10"))

    val v3d = v2.insert(using aid)(1, "20")

    val mergedOrder = v2.order.value `merge` v3d.order.value

    val mergedLists: ReplicatedList[String] = v3d `merge` v2
    val v3                                  = v2 `merge` v3d

    assertEquals(
      v3.order.value.inner.get(GrowOnlyList.Node.Head),
      mergedLists.order.value.inner.get(GrowOnlyList.Node.Head)
    )

    assertEquals(v3, mergedLists)

    assertEquals(mergedLists.order.value.toList, List(Dot(aid, 0), Dot(aid, 1)))
    assertEquals(mergedOrder.toList, List(Dot(aid, 0), Dot(aid, 1)))
    assertEquals(v3.order.value.toList, List(Dot(aid, 0), Dot(aid, 1)))

    assertEquals(v3.toList, List("10", "20"))

    val v4 = v3 `merge` v3.insert(using aid)(1, "30")

    assertEquals(v4.toList, List("10", "30", "20"))

  }

  test("purge tombstones") {

    val withPurging = {
      var l = ReplicatedList.empty[String]

      given LocalUid = LocalUid.gen()

      l = l `merge` l.append("A")
      l = l `merge` l.append("B")
      l = l `merge` l.append("C")
      l = l `merge` l.delete(1)
      l = l `merge` l.purgeTombstones()
      l = l `merge` l.delete(1)
      l
    }
    val withoutPurging = {
      var m = ReplicatedList.empty[String]

      given LocalUid = LocalUid.gen()

      m = m `merge` m.append("A")
      m = m `merge` m.append("B")
      m = m `merge` m.append("C")
      m = m `merge` m.delete(1)
      m = m `merge` m.delete(1)
      m
    }
    assertEquals(withPurging.toList, withoutPurging.toList)
  }

}
