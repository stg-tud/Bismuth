package text_rdt

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import munit.FunSuite

import scala.collection.mutable

class JsoniterCodecsTest extends FunSuite {

  test("CausalID round-trips") {
    val causalId = mutable.HashMap("A" -> 1, "B" -> 2, "C" -> 3)
    val encoded  = writeToString(causalId)
    val decoded  = readFromString[CausalID](encoded)
    assertEquals(decoded, causalId)
  }

  test("Side round-trips") {
    for side <- Side.values do
        assertEquals(readFromString[Side](writeToString(side)), side)
  }

  test("ComplexAVLMessage.Insert round-trips") {
    val insert =
      ComplexAVLMessage.Insert(
        "A",
        1,
        0,
        StringBuilder("hello"),
        "B",
        2,
        3,
        Side.Right
      )
    val decoded = readFromString[ComplexAVLMessage](writeToString(insert))
    decoded match {
      case ComplexAVLMessage.Insert(
            rid,
            counter,
            offset,
            value,
            parentRid,
            parentCounter,
            parentOffset,
            side
          ) =>
        assertEquals(rid, "A")
        assertEquals(counter, 1)
        assertEquals(offset, 0)
        assertEquals(value.toString, "hello")
        assertEquals(parentRid, "B")
        assertEquals(parentCounter, 2)
        assertEquals(parentOffset, 3)
        assertEquals(side, Side.Right)
      case other => fail(s"unexpected message: $other")
    }
  }

  test("ComplexAVLMessage.Insert with null parent round-trips") {
    val insert =
      ComplexAVLMessage.Insert(
        "A",
        0,
        0,
        StringBuilder("x"),
        null,
        0,
        0,
        Side.Left
      )
    val decoded = readFromString[ComplexAVLMessage](writeToString(insert))
    decoded match {
      case ComplexAVLMessage.Insert(_, _, _, _, parentRid, _, _, _) =>
        assertEquals(parentRid, null)
      case other => fail(s"unexpected message: $other")
    }
  }

  test("ComplexAVLMessage.Delete round-trips") {
    val delete  = ComplexAVLMessage.Delete("A", 1, 2, 3)
    val decoded = readFromString[ComplexAVLMessage](writeToString(delete))
    decoded match {
      case ComplexAVLMessage.Delete(rid, counter, offset, count) =>
        assertEquals(rid, "A")
        assertEquals(counter, 1)
        assertEquals(offset, 2)
        assertEquals(count, 3)
      case other => fail(s"unexpected message: $other")
    }
  }

  test("MyD3TreeNode round-trips recursively") {
    val tree = MyD3TreeNode(
      "root",
      Array(MyD3TreeNode("left", Array.empty, Array.empty)),
      Array(
        MyD3TreeNode(
          "right",
          Array.empty,
          Array(MyD3TreeNode("right-right", Array.empty, Array.empty))
        )
      )
    )
    val decoded = readFromString[MyD3TreeNode](writeToString(tree))
    assertEquals(writeToString(decoded), writeToString(tree))
  }

  test("head and diff collections round-trip") {
    val heads = mutable.ArrayBuffer(
      mutable.HashMap("A" -> 1),
      mutable.HashMap("B" -> 2, "C" -> 3)
    )
    assertEquals(readFromString[mutable.ArrayBuffer[CausalID]](writeToString(heads)), heads)

    val diffs: Seq[(CausalID, mutable.ArrayBuffer[ComplexAVLMessage])] = Seq(
      (
        mutable.HashMap("A" -> 1),
        mutable.ArrayBuffer(
          ComplexAVLMessage.Insert("A", 1, 0, StringBuilder("x"), null, 0, 0, Side.Left),
          ComplexAVLMessage.Delete("A", 2, 0, 1)
        )
      )
    )
    val decodedDiffs =
      readFromString[Seq[(CausalID, mutable.ArrayBuffer[ComplexAVLMessage])]](
        writeToString(diffs)
      )
    assertEquals(writeToString(decodedDiffs), writeToString(diffs))
  }

  test("session description tuple round-trips") {
    val description =
      ("offer", "sdp", Array(("candidate", "mid", 0.0)))
    val decoded =
      readFromString[(String, String, Array[(String, String, Double)])](
        writeToString(description)
      )
    assertEquals(decoded._1, description._1)
    assertEquals(decoded._2, description._2)
    assertEquals(decoded._3.toSeq, description._3.toSeq)
  }
}
