package channels

import channels.broadcast.Routing.Effect
import channels.broadcast.PlumtreeMessage.Payload
import channels.connection.{LocalMessageQueue, QueuedLocalConnection}
import channels.routing.{CausalBroadcast, CausalPlumtree}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{ReplicaId, Uid}
import rdts.time.{Causal, Dot, Dots}

import scala.collection.mutable.ArrayBuffer

class CausalBroadcastTest extends munit.FunSuite {

  given JsonValueCodec[String] = JsonCodecMaker.make

  private def node(log: ArrayBuffer[String]) = CausalBroadcast[String](ReplicaId.gen(), log.addOne)

  /** Feeds payloads as delivered by plumtree into the pure causal state, logging what it delivers. */
  private def receiveAll[A](log: ArrayBuffer[A], start: CausalPlumtree[A], payloads: Payload[Causal[A]]*)
      : CausalPlumtree[A] =
    payloads.foldLeft(start) { (state, payload) =>
      val step = state.deliver(payload)
      step.events.foreach {
        case Effect.Deliver(value) => log.addOne(value)
        case other                 => fail(s"unexpected $other")
      }
      step.state
    }

  test("holds back values until their predecessors are delivered, drops duplicates") {
    val log = ArrayBuffer.empty[String]
    val a   = Uid.predefined("a")
    val b   = Uid.predefined("b")
    val d0  = Dots.single(Dot(a, 0))
    val d1  = Dots.single(Dot(b, 0))

    var c = receiveAll(log, CausalPlumtree[String](), Payload(d1, Causal(d0, "reply")))
    assertEquals(log.toList, Nil)
    assertEquals(c.waiting, d1)

    c = receiveAll(log, c, Payload(d0, Causal(Dots.empty, "question")), Payload(d0, Causal(Dots.empty, "question")))
    assertEquals(log.toList, List("question", "reply"))
    assertEquals(c.waiting, Dots.empty)
    assertEquals(c.applied, d0.union(d1))
  }

  test("three nodes deliver in causal order") {
    val logs              = Vector.fill(3)(ArrayBuffer.empty[String])
    val Vector(n0, n1, n2) = logs.map(node)

    val queue = LocalMessageQueue()
    val link  = QueuedLocalConnection("causal", queue)
    n0.addServerConnection(link.server)
    n1.addClientConnection(link.client("1"))
    n2.addClientConnection(link.client("2"))
    while queue.nonEmpty do queue.deliverAll(): Unit

    n1.broadcast("question")
    while queue.nonEmpty do queue.deliverAll(): Unit
    n2.broadcast("answer")
    while queue.nonEmpty do queue.deliverAll(): Unit

    assertEquals(logs(0).toList, List("question", "answer"))
    assertEquals(logs(1).toList, List("answer"))
    assertEquals(logs(2).toList, List("question"))
    assertEquals(n0.state.applied, n1.state.applied)
    assertEquals(n1.state.applied, n2.state.applied)
  }

  test("merged payloads are delivered once their external predecessors are applied") {
    import rdts.base.Lattice
    given Lattice[Payload[Causal[Set[String]]]] = Lattice.derived
    val log = ArrayBuffer.empty[Set[String]]
    val a0  = Dots.single(Dot(Uid.predefined("a"), 0))
    val b0  = Dots.single(Dot(Uid.predefined("b"), 0))
    val b1  = Dots.single(Dot(Uid.predefined("b"), 1))

    val first  = Payload(b0, Causal(a0, Set("x")))
    val second = Payload(b1, Causal(a0.union(b0), Set("y")))
    val merged = Lattice.merge(first, second)

    val c = receiveAll(log, CausalPlumtree[Set[String]](), Payload(a0, Causal(Dots.empty, Set("w"))), merged, second)
    assertEquals(log.toList, List(Set("w"), Set("x", "y")))
    assertEquals(c.applied, a0.union(b0).union(b1))
  }

  test("causal codec roundtrip") {
    import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
    import CausalBroadcast.causalCodec
    val value = Causal(Dots.single(Dot(Uid.predefined("a"), 3)), "x")
    assertEquals(readFromString[Causal[String]](writeToString(value)), value)
  }
}
