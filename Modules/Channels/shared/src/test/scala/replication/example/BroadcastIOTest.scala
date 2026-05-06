package replication.example

import channels.{LocalMessageQueue, QueuedLocalConnection, SynchronousLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid
import rdts.datatypes.ReplicatedSet
import replication.BroadcastIO
import replication.JsoniterCodecs.given

class BroadcastIOTest extends munit.FunSuite {
  test("basics") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = BroadcastIO[Set[String]](LocalUid.gen(), _ => ())

    val sync = SynchronousLocalConnection()

    dd2.addBinaryConnection(sync.client("2"))
    dd1.addBinaryConnection(sync.server)
    dd3.addBinaryConnection(sync.client("3"))

    dd1.pingAll()
    dd2.pingAll()
    dd3.pingAll()

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))
    dd3.applyDelta(Set("c"))

    assertEquals(
      dd1.allPayloads.map(_.data).toSet,
      dd2.allPayloads.map(_.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.data).toSet,
      dd3.allPayloads.map(_.data).toSet
    )

  }

  test("full mesh queued 5 nodes") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val nodes = Vector.fill(5)(BroadcastIO[Set[String]](LocalUid.gen(), _ => ()))

    val queue = LocalMessageQueue()

    for
        i <- nodes.indices
        j <- (i + 1) until nodes.size
    do
        val link = QueuedLocalConnection(queue)
        nodes(i).addBinaryConnection(link.server)
        nodes(j).addBinaryConnection(link.client(s"$j->$i"))

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
      assertEquals(n.allPayloads.map(_.data).toSet, expected)
    }
  }

  test("basics queued") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val dd1, dd2, dd3 = BroadcastIO[Set[String]](LocalUid.gen(), _ => ())

    val queue  = LocalMessageQueue()
    val queued = QueuedLocalConnection(queue)

    dd2.addBinaryConnection(queued.client("2"))
    dd1.addBinaryConnection(queued.server)
    dd3.addBinaryConnection(queued.client("3"))

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
      dd1.allPayloads.map(_.data).toSet,
      dd2.allPayloads.map(_.data).toSet
    )
    assertEquals(
      dd2.allPayloads.map(_.data).toSet,
      dd3.allPayloads.map(_.data).toSet
    )

  }



  test("circle of 5 replicas converges for observe remove set operations") {

    given JsonValueCodec[String]                = JsonCodecMaker.make
    given JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]

    class Node(val uid: LocalUid) {
      var state: ReplicatedSet[String]                      = ReplicatedSet.empty[String]
      val dissemination: BroadcastIO[ReplicatedSet[String]] =
        BroadcastIO[ReplicatedSet[String]](
          uid,
          delta => state = state.merge(delta)
        )
    }

    val queue = LocalMessageQueue()

    def drainAll(nodes: => Vector[Node]): Unit =
        var safety   = 0
        var continue = true
        while continue && safety < 10000 do
            while queue.nonEmpty && safety < 10000 do
                queue.deliverAll()
                safety += 1
            val queueWasEmpty = !queue.nonEmpty
            nodes.foreach(_.dissemination.repairTick())
            continue = !queueWasEmpty || queue.nonEmpty
        assert(safety < 10000, s"queue did not quiesce, remaining=${queue.size}")

    def mkNode(): Node = new Node(LocalUid.gen())

    val nodes = Vector.fill(5)(mkNode())

    for i <- nodes.indices do
        val link = QueuedLocalConnection(queue)
        nodes(i).dissemination.addBinaryConnection(link.server)
        nodes((i + 1) % nodes.size).dissemination.addBinaryConnection(link.client(s"${i}->${(i + 1) % nodes.size}"))

    def publish(node: Node)(delta: ReplicatedSet[String]): Unit = {
      node.state = node.state.merge(delta)
      node.dissemination.applyDelta(delta)
    }

    drainAll(nodes)

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

    drainAll(nodes)

    publish(nodes(1))(nodes(1).state.remove("apple"))
    publish(nodes(3)) {
      given LocalUid = nodes(3).uid
      nodes(3).state.add("date")
    }

    drainAll(nodes)

    publish(nodes(0))(nodes(0).state.remove("carrot"))
    publish(nodes(2)) {
      given LocalUid = nodes(2).uid
      nodes(2).state.add("eggplant")
    }

    drainAll(nodes)

    val expected = Set("banana", "date", "eggplant")

    nodes.foreach { node =>
      assertEquals(node.state.elements, expected)
    }
  }

}
