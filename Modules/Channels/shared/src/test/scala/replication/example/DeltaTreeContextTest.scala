package replication.example

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.DeltaTreeContext
import replication.ProtocolMessage.Payload
import replication.{CachedMessage, SentCachedMessage}
import rdts.base.{LocalUid, Uid}
import rdts.time.{Dot, Dots, Time}
import replication.DeltaDissemination.pmscodec


class DeltaTreeContextTest extends munit.FunSuite {

  private def generateMessage(treeContext: DeltaTreeContext[Set[String]], uid: Uid, delta: Set[String]): (Dot, CachedMessage[Payload[Set[String]]]) = {
    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val nextDot = treeContext.getNextDot
    val payload = Payload(uid, Dots.single(nextDot), delta)
    val message = SentCachedMessage(payload)(using pmscodec)
    treeContext.storeOutgoingMessage(nextDot, message)
    (nextDot, message)
  }

  test("next dot") {
    val uid = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid)

    val (dot1, _) = generateMessage(treeContext, uid, Set("a"))
    val (dot2, _) = generateMessage(treeContext, uid, Set("b"))

    assertNotEquals(dot1, dot2)
  }

  test("store outgoing message single") {
    val uid = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid)
    val (nextDot, message) = generateMessage(treeContext, uid, Set("a"))

    assertEquals(treeContext.getAllPayloads.head, message)
    assertEquals(treeContext.getSelfLeaf.get, nextDot)
  }

  test("store outgoing message multiple") {
    val uid = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid)

    val (dot1, message1) = generateMessage(treeContext, uid, Set("a"))
    val (dot2, message2) = generateMessage(treeContext, uid, Set("b"))
    val (dot3, message3) = generateMessage(treeContext, uid, Set("c"))
    val (dot4, message4) = generateMessage(treeContext, uid, Set("d"))

    val payloads = treeContext.getAllPayloads
    assertEquals(payloads.size, 4)
    assertEquals(payloads.head, message1)
    assertEquals(payloads(1), message2)
    assertEquals(payloads(2), message3)
    assertEquals(payloads(3), message4)

    assertEquals(treeContext.getSelfLeaf.get, dot4)

    assertEquals(treeContext.getPredecessors(dot1), List.empty)
    assertEquals(treeContext.getPredecessors(dot2), List(dot1))
    assertEquals(treeContext.getPredecessors(dot3), List(dot2, dot1))
    assertEquals(treeContext.getPredecessors(dot4), List(dot3, dot2, dot1))
    assertEquals(treeContext.getPredecessors(uid), List(dot4, dot3, dot2, dot1))
  }

  test("add node with predecessor") {
    val uid1 = Uid.gen()
    val uid2 = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot1 = Dot(uid2, 0)
    val dot2 = dot1.advance

    treeContext.addNode(dot1)
    treeContext.addNode(dot2, Option(dot1))

    assertEquals(treeContext.getLeaf(uid1), Option.empty)
    assertEquals(treeContext.getLeaf(uid2), Option(dot2))
    assertEquals(treeContext.getPredecessors(uid2), List(dot2, dot1))
  }

  test("add node without predecessor") {
    val uid1 = Uid.gen()
    val uid2 = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot1 = Dot(uid2, 0)
    val dot2 = dot1.advance

    treeContext.addNode(dot1)
    treeContext.addNode(dot2)

    assertEquals(treeContext.getLeaf(uid1), Option.empty)
    assertEquals(treeContext.getLeaf(uid2), Option(dot2))
    assertEquals(treeContext.getPredecessors(uid2), List(dot2, dot1))
  }

  test("add node complex with predecessor") {
    /**
     * starting tree structure:
     *  (uid1,0) -- (uid1,1) -- (uid1,2)
     *  (uid2,1)
     *  (uid3,0) -- (uid3,1) -- (uid3,2)
     *
     * final tree structure:
     *  (uid1,0) -- (uid1,1) -- (uid1,2)
     *          \-- (uid2,0) -- (uid2,1)
     *  (uid3,0) -- (uid3,1) -- (uid3,2)
     */
    val uid1 = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot10 = Dot(uid1, 0)
    val dot11 = dot10.advance
    val dot12 = dot11.advance
    val uid2 = Uid.gen()
    val dot20 = Dot(uid2, 0)
    val dot21 = dot20.advance
    val uid3 = Uid.gen()
    val dot30 = Dot(uid3, 0)
    val dot31 = dot30.advance
    val dot32 = dot31.advance

    treeContext.addNode(dot10)
    treeContext.addNode(dot11, Option(dot10))
    treeContext.addNode(dot12, Option(dot11))
    treeContext.addNode(dot30)
    treeContext.addNode(dot31, Option(dot30))
    treeContext.addNode(dot32, Option(dot31))
    treeContext.addNode(dot21)

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getLeaf(uid2), Option(dot21))
    assertEquals(treeContext.getLeaf(uid3), Option(dot32))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot11, dot10))
    assertEquals(treeContext.getPredecessors(uid2), List(dot21))
    assertEquals(treeContext.getPredecessors(uid3), List(dot32, dot31, dot30))

    treeContext.addNode(dot20, Option(dot10))

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getLeaf(uid2), Option(dot21))
    assertEquals(treeContext.getLeaf(uid3), Option(dot32))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot11, dot10))
    assertEquals(treeContext.getPredecessors(uid2), List(dot21, dot20, dot10))
    assertEquals(treeContext.getPredecessors(uid3), List(dot32, dot31, dot30))
  }

  test("add node complex without predecessor") {
    /**
     * starting tree structure:
     *  (uid1,0) -- (uid1,2)
     *
     * final tree structure:
     *  (uid1,0) -- (uid1,1) -- (uid1,2)
     */
    val uid1 = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot10 = Dot(uid1, 0)
    val dot11 = dot10.advance
    val dot12 = dot11.advance

    treeContext.addNode(dot10)
    treeContext.addNode(dot12)

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot10))

    treeContext.addNode(dot11)

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot11, dot10))
  }
}
