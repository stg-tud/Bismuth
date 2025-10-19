package replication.example

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.DeltaTreeContext
import replication.DotTree
import replication.ProtocolMessage.Payload
import replication.{CachedMessage, SentCachedMessage}
import rdts.base.{LocalUid, Uid}
import rdts.time.{Dot, Dots, Time}
import replication.DeltaDissemination.pmscodec

class DeltaTreeContextTest extends munit.FunSuite {

  private def generateMessage(
      treeContext: DeltaTreeContext[Set[String]],
      uid: Uid,
      delta: Set[String]
  ): (Dot, CachedMessage[Payload[Set[String]]]) = {
    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val nextDot = treeContext.getNextDot
    val payload = Payload(uid, Dots.single(nextDot), delta)
    val message = SentCachedMessage(payload)(using pmscodec)
    treeContext.storeOutgoingMessage(nextDot, message)
    (nextDot, message)
  }

  test("next dot") {
    val uid                                        = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid)

    val (dot1, _) = generateMessage(treeContext, uid, Set("a"))
    val (dot2, _) = generateMessage(treeContext, uid, Set("b"))

    assertNotEquals(dot1, dot2)
  }

  test("store outgoing message single") {
    val uid                                        = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid)
    val (nextDot, message)                         = generateMessage(treeContext, uid, Set("a"))

    assertEquals(treeContext.getAllPayloads.head, message)
    assertEquals(treeContext.getSelfLeaf.get, nextDot)
  }

  test("store outgoing message multiple") {
    val uid                                        = Uid.gen()
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

  test("add node") {
    val uid1                                       = Uid.gen()
    val uid2                                       = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot1                                       = Dot(uid2, 0)
    val dot2                                       = dot1.advance

    treeContext.addNode(dot1, dot1)
    treeContext.addNode(dot2, dot1)

    assertEquals(treeContext.getLeaf(uid1), Option.empty)
    assertEquals(treeContext.getLeaf(uid2), Option(dot2))
    assertEquals(treeContext.getPredecessors(uid2), List(dot2, dot1))
  }

  test("add node complex with missing dot") {

    /** starting tree structure:
      *  root -- (uid1,0) -- (uid1,1) -- (uid1,2)
      *      \-- (uid2,1)
      *       \- (uid3,0) -- (uid3,1) -- (uid3,2)
      *
      * final tree structure:
      *  root -- (uid1,0) -- (uid1,1) -- (uid1,2)
      *      \-- (uid2,0) -- (uid2,1)
      *       \- (uid3,0) -- (uid3,1) -- (uid3,2)
      */
    val uid1                                       = Uid.gen()
    val treeContext: DeltaTreeContext[Set[String]] = DeltaTreeContext[Set[String]](uid1)
    val dot10                                      = Dot(uid1, 0)
    val dot11                                      = dot10.advance
    val dot12                                      = dot11.advance
    val uid2                                       = Uid.gen()
    val dot20                                      = Dot(uid2, 0)
    val dot21                                      = dot20.advance
    val uid3                                       = Uid.gen()
    val dot30                                      = Dot(uid3, 0)
    val dot31                                      = dot30.advance
    val dot32                                      = dot31.advance

    treeContext.addNode(dot10, dot10)
    treeContext.addNode(dot11, dot10)
    treeContext.addNode(dot12, dot11)
    treeContext.addNode(dot30, dot30)
    treeContext.addNode(dot31, dot30)
    treeContext.addNode(dot32, dot31)
    treeContext.addNode(dot21, dot20)

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getLeaf(uid2), Option(dot21))
    assertEquals(treeContext.getLeaf(uid3), Option(dot32))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot11, dot10))
    assertEquals(treeContext.getPredecessors(uid2), List(dot21))
    assertEquals(treeContext.getPredecessors(uid3), List(dot32, dot31, dot30))
    assertEquals(treeContext.tree.causalBarriers.values.map(_.dot).toSet, Set(dot12, dot32))

    treeContext.addNode(dot20, dot20)

    assertEquals(treeContext.getLeaf(uid1), Option(dot12))
    assertEquals(treeContext.getLeaf(uid2), Option(dot21))
    assertEquals(treeContext.getLeaf(uid3), Option(dot32))
    assertEquals(treeContext.getPredecessors(uid1), List(dot12, dot11, dot10))
    assertEquals(treeContext.getPredecessors(uid2), List(dot21, dot20))
    assertEquals(treeContext.getPredecessors(uid3), List(dot32, dot31, dot30))
    assertEquals(treeContext.tree.causalBarriers.values.map(_.dot).toSet, Set(dot12, dot21, dot32))
  }

  test("get unknown dots for peer with empty knowledge") {
    val uid     = Uid.gen()
    val tree    = DotTree()
    var dots    = Dots.empty
    var prevDot = dots.nextDot(uid)
    tree.addNode(prevDot, prevDot)
    dots = dots.add(prevDot)
    for i <- 0 until 9 do {
      val dot = dots.nextDot(uid)
      tree.addNode(dot, prevDot)
      dots = dots.add(dot)
      prevDot = dot
    }

    assertEquals(tree.getUnknownDotsForPeer(Uid.gen(), Dots.empty), dots)
  }

  test("get unknown dots for peer with knowledge") {
    val uid     = Uid.gen()
    val peer    = Uid.gen()
    val tree    = DotTree()
    var dots    = Dots.empty
    var known   = Dots.empty
    var unknown = Dots.empty
    var prevDot = dots.nextDot(uid)
    tree.addNode(prevDot, prevDot)
    dots = dots.add(prevDot)
    for i <- 0 until 9 do {
      val dot = dots.nextDot(uid)
      tree.addNode(dot, prevDot)
      dots = dots.add(dot)
      prevDot = dot
      if i == 2 then known = known.add(dot) else if i > 2 then unknown = unknown.add(dot)
    }

    assertEquals(tree.getUnknownDotsForPeer(peer, known), unknown)
    assertEquals(tree.getUnknownDotsForPeer(peer, unknown), Dots.empty)
  }

  test("get unknown dots for peer with knowledge complex") {
    val uid1     = Uid.gen()
    val uid2     = Uid.gen()
    val uid3     = Uid.gen()
    val peer     = Uid.gen()
    val tree     = DotTree()
    var dots     = Dots.empty
    var known    = Dots.empty
    var unknown  = Dots.empty
    var prevDot1 = dots.nextDot(uid1)
    tree.addNode(prevDot1, prevDot1)
    dots = dots.add(prevDot1)
    var prevDot2 = dots.nextDot(uid2)
    tree.addNode(prevDot2, prevDot2)
    dots = dots.add(prevDot2)
    var prevDot3 = dots.nextDot(uid3)
    tree.addNode(prevDot3, prevDot3)
    dots = dots.add(prevDot3)
    unknown = unknown.add(prevDot3)
    for i <- 0 until 10 do {
      val dot1 = dots.nextDot(uid1)
      tree.addNode(dot1, prevDot1)
      prevDot1 = dot1
      dots = dots.add(dot1)
      val dot2 = dots.nextDot(uid2)
      tree.addNode(dot2, prevDot2)
      prevDot2 = dot2
      dots = dots.add(dot2)
      val dot3 = dots.nextDot(uid3)
      tree.addNode(dot3, prevDot3)
      prevDot3 = dot3
      dots = dots.add(dot3)

      if i == 2 then known = known.add(dot1) else if i > 2 then unknown = unknown.add(dot1)
      if i == 4 then known = known.add(dot2) else if i > 4 then unknown = unknown.add(dot2)
      unknown = unknown.add(dot3)
    }

    assertEquals(tree.getUnknownDotsForPeer(peer, known), unknown)
    assertEquals(tree.getUnknownDotsForPeer(peer, unknown), Dots.empty)
  }

  test("knowledge of peers simple") {
    val uid     = Uid.gen()
    val tree    = DotTree()
    var dots    = Dots.empty
    var prevDot = dots.nextDot(uid)
    tree.addNode(prevDot, prevDot)
    dots = dots.add(prevDot)
    for i <- 0 until 9 do {
      val dot = dots.nextDot(uid)
      tree.addNode(dot, prevDot)
      dots = dots.add(dot)
      prevDot = dot
    }

    val peerUid     = Uid.gen()
    var peerPrevDot = dots.nextDot(peerUid)
    tree.addNode(peerPrevDot, peerPrevDot)
    dots = dots.add(peerPrevDot)

    dots.peers.foreach { peer =>
      tree.updateKnowledgeOfPeer(peerUid, dots.clockOf(peer).get)
    }
    tree.collapseGeneralKnowledge()

    assertEquals(tree.leaves.values.toSet, tree.rootNode.successors)
  }

  test("knowledge of peers with missing knowledge") {
    val uid     = Uid.gen()
    val tree    = DotTree()
    var dots    = Dots.empty
    var prevDot = dots.nextDot(uid)
    tree.addNode(prevDot, prevDot)
    dots = dots.add(prevDot)
    for i <- 0 until 9 do {
      val dot = dots.nextDot(uid)
      if (i % 2) == 0 then tree.addNode(dot, prevDot)
      dots = dots.add(dot)
      prevDot = dot
    }

    val peerUid     = Uid.gen()
    var peerPrevDot = dots.nextDot(peerUid)
    tree.addNode(peerPrevDot, peerPrevDot)
    dots = dots.add(peerPrevDot)

    dots.peers.foreach { peer =>
      tree.updateKnowledgeOfPeer(peerUid, dots.clockOf(peer).get)
    }
    tree.collapseGeneralKnowledge()

    assertEquals(tree.leaves.values.toDots, Dots.empty.add(prevDot).add(peerPrevDot))
    assertEquals(tree.rootNode.successors.toDots, Dots.empty.add(Dot(uid, 1)).add(peerPrevDot))
  }

  test("add dots with missing knowledge") {
    val uid                    = Uid.gen()
    val tree                   = DotTree()
    var dots                   = Dots.empty
    var expected               = Dots.empty
    var expectedCausalBarriers = Dots.empty
    var prevDot                = dots.nextDot(uid)
    tree.addNode(prevDot, prevDot)
    dots = dots.add(prevDot)
    expected = expected.add(prevDot)
    for i <- 0 until 9 do {
      val dot = dots.nextDot(uid)
      if (i % 2) == 0 then {
        tree.addNode(dot, prevDot)
        expected = expected.add(dot)
      }
      if i == 0 then expectedCausalBarriers = expectedCausalBarriers.add(dot)
      dots = dots.add(dot)
      prevDot = dot
    }

    assertEquals(tree.collapse, expected)
    assertEquals(tree.leaves.values.map(node => node.dot).toSet, Set(prevDot))
    assertEquals(
      tree.causalBarriers.values.foldLeft(Dots.empty)((dots, node) => dots.add(node.dot)),
      expectedCausalBarriers
    )
  }

  // TODO: test for duplicates
}
