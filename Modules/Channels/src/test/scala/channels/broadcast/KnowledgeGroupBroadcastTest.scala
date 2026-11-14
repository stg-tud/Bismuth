package channels.broadcast

import channels.JsoniterCodecs.given
import channels.MessagingIO
import channels.broadcast.KnowledgeGroupBroadcast.{Delivery, Message, State, publish}
import channels.broadcast.Routing.{Effect, Step}
import channels.connection.{LocalMessageQueue, Peer, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{ReplicaId, Uid}
import rdts.time.{Dot, Dots}

import scala.collection.mutable.ArrayBuffer

/** Tests for [[KnowledgeGroupBroadcast]]: which deltas are sent to whom, and what a receiving replica
  * delivers and forwards. The tests drive the protocol state machine directly, apart from one test that
  * runs it through [[channels.MessagingIO]] and local connections.
  */
class KnowledgeGroupBroadcastTest extends munit.FunSuite {

  given JsonValueCodec[Int] = JsonCodecMaker.make

  private val a = Uid.predefined("a")
  private val b = Uid.predefined("b")
  private val c = Uid.predefined("c")
  private val d = Uid.predefined("d")

  private type Result = Step[State, Message[Set[Int]], Delivery[Set[Int]]]

  /** Group matching only the even part of a delta. */
  private def evenGroup(members: Uid*) =
    KnowledgeGroup[Set[Int]](members.toSet, _.filter(_ % 2 == 0), _ => true)

  /** Group matching only the odd part of a delta. */
  private def oddGroup(members: Uid*) =
    KnowledgeGroup[Set[Int]](members.toSet, _.filter(_ % 2 != 0), _ => true)

  private def allGroup(members: Uid*) =
    KnowledgeGroup[Set[Int]](members.toSet, identity, _ => true)

  private def protocolFor(id: Uid, groups: KnowledgeGroup[Set[Int]]*): KnowledgeGroupBroadcast.Protocol[Set[Int]] =
      given ReplicaId = ReplicaId(id)
      new KnowledgeGroupBroadcast.Protocol[Set[Int]](PrdtSystem(groups.toSet))

  private def stateWithPeers(ids: Uid*): State = State(peers = ids.map(Peer.Claimed(_)).toSet)

  private def sent(step: Result): List[(Set[Uid], Message[Set[Int]])] =
    step.events.toList.collect { case Effect.Send(peers, message) => (peers.map(_.uid).toSet, message) }

  private def delivered(step: Result): List[Delivery[Set[Int]]] =
    step.events.toList.collect { case Effect.Deliver(delivery) => delivery }

  // ------------------------------------------------------------------------------------------------
  // knowledge groups
  // ------------------------------------------------------------------------------------------------

  test("a group matches only deltas with a non-empty relevant part, and only if enabled") {
    val group = KnowledgeGroup[Set[Int]](Set(a, b), _.filter(_ % 2 == 0), _.size < 3)
    assert(group.matches(Set(2)), "even delta matches")
    assert(!group.matches(Set(1)), "odd delta has an empty relevant part")
    assert(!group.matches(Set(2, 4, 6)), "disabled for deltas of size >= 3")
  }

  // ------------------------------------------------------------------------------------------------
  // publishing
  // ------------------------------------------------------------------------------------------------

  test("publish sends to the peers of matching groups only") {
    val protocol = protocolFor(a, evenGroup(a, b), oddGroup(a, c))
    val state    = stateWithPeers(b, c, d)

    val (step, dot) = protocol.publish(a, Set(2))(state)

    assertEquals(dot, Dot(a, 0))
    assertEquals(sent(step), List(Set(b) -> Message(Dots.single(dot), Set(2))))
    assertEquals(step.state.context, Dots.single(dot))
  }

  test("publish selects the peers by the group whose relevant part is non-empty") {
    val protocol = protocolFor(a, evenGroup(a, b), oddGroup(a, c))
    val state    = stateWithPeers(b, c)

    val (step, dot) = protocol.publish(a, Set(1, 2))(state)

    assertEquals(sent(step).map((targets, message) => targets -> message.dots), List(Set(b, c) -> Dots.single(dot)))
  }

  test("publish assigns consecutive dots") {
    val protocol = protocolFor(a, evenGroup(a, b))

    val (step1, dot1) = protocol.publish(a, Set(2))(stateWithPeers(b))
    val (step2, dot2) = protocol.publish(a, Set(4))(step1.state)

    assertEquals(dot1, Dot(a, 0))
    assertEquals(dot2, Dot(a, 1))
    assertEquals(step2.state.context, Dots.single(dot1).union(Dots.single(dot2)))
    assertEquals(sent(step2), List(Set(b) -> Message(Dots.single(dot2), Set(4))))
  }

  test("publish sends nothing to peers outside all matching groups") {
    val protocol = protocolFor(a, evenGroup(a, b))

    val (step, dot) = protocol.publish(a, Set(2))(stateWithPeers(c, d))

    assertEquals(dot, Dot(a, 0))
    assertEquals(sent(step), Nil)
    assertEquals(delivered(step), Nil)
    assertEquals(step.state.context, Dots.single(dot), "the delta is still recorded as published")
  }

  // ------------------------------------------------------------------------------------------------
  // receiving
  // ------------------------------------------------------------------------------------------------

  test("a received delta is delivered and forwarded to matching neighbors") {
    val protocol = protocolFor(b, allGroup(a, b), allGroup(b, c))
    val message  = Message(Dots.single(Dot(a, 0)), Set(1))

    val step = protocol.handleMessage(stateWithPeers(a, c), Peer.Claimed(a), message)

    assertEquals(delivered(step), List(Delivery(message.dots, message.data)))
    assertEquals(sent(step), List(Set(c) -> message))
    assertEquals(step.state.context, message.dots)
  }

  test("the delta is not forwarded to the group it was received through") {
    val protocol = protocolFor(b, allGroup(a, b), allGroup(b, c))
    val message  = Message(Dots.single(Dot(a, 0)), Set(1))

    val step = protocol.handleMessage(stateWithPeers(a, c), Peer.Claimed(a), message)

    assertEquals(sent(step).map(_._1), List(Set(c)), "the sender a is covered by the shared group")
  }

  test("a delta matching no group shared with any neighbor is delivered but not forwarded") {
    val protocol = protocolFor(b, evenGroup(a, b), oddGroup(b, c))
    val message  = Message(Dots.single(Dot(a, 0)), Set(2))

    val step = protocol.handleMessage(stateWithPeers(a, c), Peer.Claimed(a), message)

    assertEquals(delivered(step), List(Delivery(message.dots, message.data)))
    assertEquals(sent(step).map(_._1), Nil, "the odd group does not match an even delta")
  }

  test("a duplicate delta is delivered once") {
    val protocol = protocolFor(b, allGroup(a, b))
    val message  = Message(Dots.single(Dot(a, 0)), Set(1))

    val first  = protocol.handleMessage(stateWithPeers(a), Peer.Claimed(a), message)
    val second = protocol.handleMessage(first.state, Peer.Claimed(a), message)

    assertEquals(delivered(first).size, 1)
    assertEquals(second.events.toList, Nil)
  }

  test("a delta whose dots are already known is ignored") {
    val protocol = protocolFor(b, allGroup(a, b))
    val known    = Dots.single(Dot(a, 0)).union(Dots.single(Dot(a, 1)))
    val state    = stateWithPeers(a).copy(context = known)

    val step = protocol.handleMessage(state, Peer.Claimed(a), Message(Dots.single(Dot(a, 0)), Set(1)))

    assertEquals(step.events.toList, Nil)
    assertEquals(step.state, state)
  }

  test("only groups of the immediate sender are covered, not those of earlier hops") {
    // a -> b -> c in a triangle: c shares a group with a, but did not receive the delta through it
    val protocol = protocolFor(c, allGroup(a, b), allGroup(b, c), allGroup(a, c))
    val message  = Message(Dots.single(Dot(a, 0)), Set(1))

    val step = protocol.handleMessage(stateWithPeers(a, b), Peer.Claimed(b), message)

    // current behaviour: c forwards back to a, which drops the duplicate because of its context
    assertEquals(sent(step).map(_._1), List(Set(a)))
  }

  // ------------------------------------------------------------------------------------------------
  // codec and driver
  // ------------------------------------------------------------------------------------------------

  test("message codec roundtrip") {
    given JsonValueCodec[Message[Set[Int]]] = KnowledgeGroupBroadcast.messageCodec[Set[Int]]
    val message                             = Message(Dots.single(Dot(a, 3)).union(Dots.single(Dot(b, 0))), Set(1, 2))

    assertEquals(readFromArray[Message[Set[Int]]](writeToArray(message)), message)
  }

  test("publish propagates through a relay to members of another group") {
    val bridge = Uid.predefined("bridge")
    val left   = Uid.predefined("left")
    val right  = Uid.predefined("right")

    val system = PrdtSystem(Set(allGroup(bridge, left), allGroup(bridge, right)))
    val queue  = LocalMessageQueue()
    val link   = QueuedLocalConnection("knowledge-groups", queue)

    def node(id: Uid): (MessagingIO[KnowledgeGroupBroadcast.Protocol[Set[Int]]], ArrayBuffer[Delivery[Set[Int]]]) =
        given ReplicaId = ReplicaId(id)
        val log         = ArrayBuffer.empty[Delivery[Set[Int]]]
        val io          = new MessagingIO[KnowledgeGroupBroadcast.Protocol[Set[Int]]](
          using
          new KnowledgeGroupBroadcast.Protocol[Set[Int]](system),
          KnowledgeGroupBroadcast.messageCodec[Set[Int]],
        )(ReplicaId(id), State(), delivery => { log.addOne(delivery); () })
        (io, log)

    def drain(): Unit = while queue.nonEmpty do { queue.deliverAll(); () }

    val (bridgeNode, bridgeLog) = node(bridge)
    val (leftNode, leftLog)     = node(left)
    val (rightNode, rightLog)   = node(right)

    bridgeNode.addServerConnection(link.server)
    leftNode.addClientConnection(link.client("left"))
    rightNode.addClientConnection(link.client("right"))
    drain()

    assertEquals(leftNode.state.peers.map(_.uid), Set(bridge))
    assertEquals(rightNode.state.peers.map(_.uid), Set(bridge))

    leftNode.publish(Set(1))
    drain()

    assertEquals(leftLog.map(_.data).toList, Nil, "the publisher does not deliver locally")
    assertEquals(bridgeLog.map(_.data).toList, List(Set(1)))
    assertEquals(rightLog.map(_.data).toList, List(Set(1)), "reached through the bridge")
  }
}
