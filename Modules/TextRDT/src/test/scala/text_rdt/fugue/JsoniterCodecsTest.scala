package text_rdt.fugue

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import munit.FunSuite
import text_rdt.debug.{*, given}
import text_rdt.fugue.complex.{*, given}
import text_rdt.fugue.complexavl.{*, given}
import text_rdt.fugue.simple.{*, given}
import text_rdt.fugue.simpleavl.{*, given}
import text_rdt.{*, given}

class JsoniterCodecsTest extends FunSuite {

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

  test("Fugue operations round-trip") {
    val simple: Seq[Message[SimpleID]] = Seq(
      Message.Insert(SimpleID("A", 1), 'x', null, Side.Right),
      Message.Insert(SimpleID("B", 2), 'y', SimpleID("A", 1), Side.Left),
      Message.Delete(SimpleID("A", 1))
    )
    simple.foreach(msg =>
      assertEquals(readFromString(writeToString(msg)(using simpleMessageCodec))(using simpleMessageCodec), msg)
    )

    val complex: Seq[Message[ComplexID]] = Seq(
      Message.Insert(ComplexID("A", 1, 0), 'x', null, Side.Right),
      Message.Insert(ComplexID("B", 2, 3), 'y', ComplexID("A", 1, 0), Side.Left),
      Message.Delete(ComplexID("A", 1, 0))
    )
    complex.foreach(msg =>
      assertEquals(readFromString(writeToString(msg)(using complexMessageCodec))(using complexMessageCodec), msg)
    )
  }
}
