package replication.example

import channels.{LocalMessageQueue, QueuedLocalConnection, SynchronousLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Lattice.syntax
import rdts.base.LocalUid
import rdts.datatypes.ReplicatedSet

import replication.JsoniterCodecs.given
import replication.{DeltaDisseminationFactory, PlumtreeDissemination, ProtocolMessage}

class PlumtreeDisseminationTest extends munit.FunSuite {
  test("basics") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None)

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    dd2.addObjectConnection(sync.client("2"))
    dd1.addObjectConnection(sync.server)
    dd3.addObjectConnection(sync.client("3"))

    dd1.pingAll()
    dd2.pingAll()
    dd3.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))
    dd3.applyDelta(Set("c"))

    assertEquals(
      dd1.allPayloads.map(_.payload.data).toSet,
      dd2.allPayloads.map(_.payload.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.payload.data).toSet,
      dd3.allPayloads.map(_.payload.data).toSet
    )

  }

  test("full mesh queued 5 nodes") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val nodes = Vector.fill(5)(PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None))

    val queue = LocalMessageQueue[ProtocolMessage[Set[String]]]()

    for
        i <- nodes.indices
        j <- (i + 1) until nodes.size
    do
      val link = QueuedLocalConnection[ProtocolMessage[Set[String]]](queue)
      nodes(i).addObjectConnection(link.server)
      nodes(j).addObjectConnection(link.client(s"$j->$i"))

    // drain connection setup traffic (initial requests/replies)
    var setupSafety = 0
    while queue.nonEmpty && setupSafety < 10000 do
      queue.deliverAll()
      setupSafety += 1

    nodes.zipWithIndex.foreach { case (n, idx) =>
      n.applyDelta(Set(s"v$idx"))
    }

    var disseminateSafety = 0
    while queue.nonEmpty && disseminateSafety < 10000 do
      queue.deliverAll()
      disseminateSafety += 1

    val expected = Set(Set("v0"), Set("v1"), Set("v2"), Set("v3"), Set("v4"))

    nodes.foreach { n =>
      assertEquals(n.allPayloads.map(_.payload.data).toSet, expected)
    }
  }

  test("basics queued") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val dd1, dd2, dd3 = PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None)

    val queue  = LocalMessageQueue[ProtocolMessage[Set[String]]]()
    val queued = QueuedLocalConnection[ProtocolMessage[Set[String]]](queue)

    dd2.addObjectConnection(queued.client("2"))
    dd1.addObjectConnection(queued.server)
    dd3.addObjectConnection(queued.client("3"))

    def deliverAndPrint(): Unit =
      val log = false
      if log then
        println("delivering:")
        queue.elements.foreach(e => println(s"  $e"))
      queue.deliverAll()
      if log then
        println("enqueued: ")
        queue.elements.foreach(e => println(s"  $e"))
        println("")

    deliverAndPrint()
    dd1.pingAll()
    deliverAndPrint()
    dd2.pingAll()
    dd3.pingAll()
    deliverAndPrint()



    dd1.applyDelta(Set("a"))
    deliverAndPrint()
    dd2.applyDelta(Set("b"))
    deliverAndPrint()

    dd3.applyDelta(Set("c"))
    deliverAndPrint()
    deliverAndPrint()
    deliverAndPrint()

    assertEquals(
      dd1.allPayloads.map(_.payload.data).toSet,
      dd2.allPayloads.map(_.payload.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.payload.data).toSet,
      dd3.allPayloads.map(_.payload.data).toSet
    )

  }

  test("factory interface can create an applyDelta handle") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val sync = SynchronousLocalConnection[ProtocolMessage[Set[String]]]()

    val setup: DeltaDisseminationFactory[Set[String]] =
      PlumtreeDissemination.factory(LocalUid.gen())(_.addObjectConnection(sync.server))

    var received = List.empty[Set[String]]
    val dissemination = setup.bind(delta => received = delta :: received)

    val remote = PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None)
    remote.addObjectConnection(sync.client("remote"))

    remote.applyDelta(Set("hello"))

    assertEquals(received.reverse, List(Set("hello")))
  }

  test("circle of 5 replicas converges for observe remove set operations") {

    given JsonValueCodec[String] = JsonCodecMaker.make
    given JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]

    class Node(val uid: LocalUid) {
      var state: ReplicatedSet[String] = ReplicatedSet.empty[String]
      val dissemination: PlumtreeDissemination[ReplicatedSet[String]] =
        PlumtreeDissemination[ReplicatedSet[String]](
          uid,
          delta => state = state.merge(delta),
          None
        )
    }

    val queue = LocalMessageQueue[ProtocolMessage[ReplicatedSet[String]]]()

    def drainAll(): Unit =
      var safety = 0
      while queue.nonEmpty && safety < 10000 do
        queue.deliverAll()
        safety += 1

    def mkNode(): Node = new Node(LocalUid.gen())

    val nodes = Vector.fill(5)(mkNode())

    for i <- nodes.indices do
      val link = QueuedLocalConnection[ProtocolMessage[ReplicatedSet[String]]](queue)
      nodes(i).dissemination.addObjectConnection(link.server)
      nodes((i + 1) % nodes.size).dissemination.addObjectConnection(link.client(s"${i}->${(i + 1) % nodes.size}"))

    def publish(node: Node)(delta: ReplicatedSet[String]): Unit = {
      node.state = node.state.merge(delta)
      node.dissemination.applyDelta(delta)
    }

    drainAll()

    publish(nodes(0)) {
      given LocalUid = nodes(0).uid
      nodes(0).state.add("apple")
    }
    publish(nodes(2)) {
      given LocalUid = nodes(2).uid
      nodes(2).state.add("banana")
    }
    publish(nodes(4)) {
      given LocalUid = nodes(4).uid
      nodes(4).state.add("carrot")
    }

    drainAll()

    publish(nodes(1))(nodes(1).state.remove("apple"))
    publish(nodes(3)) {
      given LocalUid = nodes(3).uid
      nodes(3).state.add("date")
    }

    drainAll()

    publish(nodes(0))(nodes(0).state.remove("carrot"))
    publish(nodes(2)) {
      given LocalUid = nodes(2).uid
      nodes(2).state.add("eggplant")
    }

    drainAll()

    val expected = Set("banana", "date", "eggplant")

    nodes.foreach { node =>
      assertEquals(node.state.elements, expected)
    }
  }

}
