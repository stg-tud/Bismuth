package replication

import channels.{ConnectionDescriptor, LocalConnectionRegistry, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection, SynchronousLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid
import rdts.datatypes.ReplicatedSet
import replication.JsoniterCodecs.given
import replication.overlay.FullMeshOverlay

class BroadcastIOTest extends munit.FunSuite {
  test("basics") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2, dd3 = {
      val uid = LocalUid.gen()
      BroadcastIO[Set[String]](uid, _ => ())
    }

    val sync = SynchronousLocalConnection("sync-basics")

    dd2.addClientConnection(sync.client("2"))
    dd1.addServerConnection(sync.server)
    dd3.addClientConnection(sync.client("3"))

    dd1.broadcast(Set("a"))
    dd2.broadcast(Set("b"))
    dd3.broadcast(Set("c"))

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
        val link = QueuedLocalConnection(s"queued-$i-$j", queue)
        nodes(i).addServerConnection(link.server)
        nodes(j).addClientConnection(link.client(s"$j->$i"))

    // drain connection setup traffic (initial requests/replies)
    var setupSafety = 0
    while queue.nonEmpty && setupSafety < 10000 do
        queue.deliverAll()
        setupSafety += 1

    nodes.zipWithIndex.foreach { case (n, idx) =>
      n.broadcast(Set(s"v$idx"))
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
    val queued = QueuedLocalConnection("queued-basics", queue)

    dd2.addClientConnection(queued.client("2"))
    dd1.addServerConnection(queued.server)
    dd3.addClientConnection(queued.client("3"))

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
    deliverAndPrint()
    deliverAndPrint()

    dd1.broadcast(Set("a"))
    deliverAndPrint()
    dd2.broadcast(Set("b"))
    deliverAndPrint()

    dd3.broadcast(Set("c"))
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

  test("discover + queued-local connection info forms connections and disseminates without direct wiring") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val queue = LocalMessageQueue()
    val links = Map(
      "n0" -> QueuedLocalConnection("n0", queue),
      "n1" -> QueuedLocalConnection("n1", queue),
      "n2" -> QueuedLocalConnection("n2", queue),
    )
    val resolver = LocalConnectionRegistry(links)

    final case class Node(id: String) {
      val uid: LocalUid = LocalUid.gen()
      val selfInfo      = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      val io            = BroadcastIO[Set[String]](
        uid,
        _ => (),
        overlay = Some(FullMeshOverlay(selfInfo)),
        resolver = resolver,
      )
    }

    val nodes = Vector(Node("n0"), Node("n1"), Node("n2"))

    nodes.foreach { node =>
      node.io.addServerConnection(resolver.queuedServer(node.selfInfo.channelConnectors.head).get)
    }

    nodes.foreach { node =>
      val others = nodes.filterNot(_.uid == node.uid).map(_.selfInfo).toSet
      node.io.discover(others)
    }

    def drain(maxRounds: Int = 100): Unit = {
      var rounds = 0
      while queue.nonEmpty && rounds < maxRounds do
          queue.deliverAll()
          rounds += 1
      assert(rounds < maxRounds, s"queue did not quiesce, remaining=${queue.size}")
    }

    drain()

    nodes.zipWithIndex.foreach { case (node, idx) =>
      node.io.broadcast(Set(s"v$idx"))
      drain()
    }

    val expected = Set(Set("v0"), Set("v1"), Set("v2"))
    nodes.foreach { node =>
      assertEquals(node.io.allPayloads.map(_.data).toSet, expected)
    }
  }

  test("late joins backfill history and disconnected nodes stop participating") {

    given JsonValueCodec[Set[String]] = JsonCodecMaker.make

    val queue = LocalMessageQueue()
    val links = Map(
      "n0" -> QueuedLocalConnection("n0", queue),
      "n1" -> QueuedLocalConnection("n1", queue),
      "n2" -> QueuedLocalConnection("n2", queue),
      "n3" -> QueuedLocalConnection("n3", queue),
    )
    val resolver = LocalConnectionRegistry(links)

    final case class Node(id: String) {
      val uid: LocalUid = LocalUid.gen()
      val selfInfo      = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      val io            = BroadcastIO[Set[String]](
        uid,
        _ => (),
        overlay = Some(FullMeshOverlay(selfInfo)),
        resolver = resolver,
      )

      def startListening(): Unit =
        io.addServerConnection(resolver.queuedServer(selfInfo.channelConnectors.head).get)

      def discover(peers: Iterable[Node]): Unit =
        io.discover(peers.iterator.map(_.selfInfo).toSet)

      def closeConnectionTo(other: Node): Unit =
        io.overlayController.asInstanceOf[FullMeshOverlay].active.get(other.uid.uid).foreach(_.close())
    }

    def drain(maxRounds: Int = 200): Unit = {
      var rounds = 0
      while queue.nonEmpty && rounds < maxRounds do
          queue.deliverAll()
          rounds += 1
      assert(rounds < maxRounds, s"queue did not quiesce, remaining=${queue.size}")
    }

    val n0 = Node("n0")
    val n1 = Node("n1")
    val n2 = Node("n2")
    val n3 = Node("n3")

    Vector(n0, n1, n2).foreach(_.startListening())
    Vector(n0, n1, n2).foreach { node =>
      node.discover(Vector(n0, n1, n2).filterNot(_ == node))
    }
    drain()

    n0.io.broadcast(Set("before-0"))
    drain()
    n1.io.broadcast(Set("before-1"))
    drain()

    val initialExpected = Set(Set("before-0"), Set("before-1"))
    Vector(n0, n1, n2).foreach { node =>
      assertEquals(node.io.allPayloads.map(_.data).toSet, initialExpected)
    }

    n3.startListening()
    n3.discover(Vector(n0, n1, n2))
    Vector(n0, n1, n2).foreach(_.discover(Vector(n3)))
    drain()

    assertEquals(n3.io.allPayloads.map(_.data).toSet, initialExpected)

    Vector(n0, n2, n3).foreach(_.closeConnectionTo(n1))
    n1.closeConnectionTo(n0)
    n1.closeConnectionTo(n2)
    n1.closeConnectionTo(n3)
    drain()

    n0.io.broadcast(Set("after-0"))
    drain()
    n3.io.broadcast(Set("after-3"))
    drain()

    val survivingExpected = initialExpected ++ Set(Set("after-0"), Set("after-3"))
    Vector(n0, n2, n3).foreach { node =>
      assertEquals(node.io.allPayloads.map(_.data).toSet, survivingExpected)
    }

    assertEquals(n1.io.allPayloads.map(_.data).toSet, initialExpected)
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
            nodes.foreach(_.dissemination.tick())
            continue = !queueWasEmpty || queue.nonEmpty
        assert(safety < 10000, s"queue did not quiesce, remaining=${queue.size}")

    def mkNode(): Node = new Node(LocalUid.gen())

    val nodes = Vector.fill(5)(mkNode())

    for i <- nodes.indices do
        val link = QueuedLocalConnection(s"circle-$i", queue)
        nodes(i).dissemination.addServerConnection(link.server)
        nodes((i + 1) % nodes.size).dissemination.addClientConnection(link.client(s"${i}->${(i + 1) % nodes.size}"))

    def publish(node: Node)(delta: ReplicatedSet[String]): Unit = {
      node.state = node.state.merge(delta)
      node.dissemination.broadcast(delta)
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
