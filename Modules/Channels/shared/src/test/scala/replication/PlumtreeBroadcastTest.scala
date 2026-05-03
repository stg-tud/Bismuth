package replication

import munit.FunSuite
import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event
import replication.PlumtreeBroadcast.Peer
import replication.PlumtreeMessage.Payload

import scala.collection.mutable

class PlumtreeBroadcastTest extends FunSuite {

  private case class WireMessage(from: String, to: String, message: PlumtreeMessage[String])

  private class Network(initialNodes: Iterable[String]) {
    private val ids = mutable.LinkedHashSet.from(initialNodes)
    private val adjacency = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]
    private val queue = mutable.Queue.empty[WireMessage]
    private var nodes = ids.iterator.map(id => id -> freshNode(id)).toMap

    ids.foreach(id => adjacency.update(id, mutable.LinkedHashSet.empty))

    private def freshNode(id: String): PlumtreeBroadcast[String] =
      PlumtreeBroadcast(Uid.predefined(id), deltaStorage = KeepAllHistory())

    def addNode(id: String): Unit =
      if !ids.contains(id) then
        ids += id
        adjacency.update(id, mutable.LinkedHashSet.empty)
        nodes = nodes.updated(id, freshNode(id))

    def addLink(a: String, b: String): Unit = {
      addNode(a)
      addNode(b)
      adjacency(a) += b
      adjacency(b) += a
      applyResult(a, nodes(a).addPeer(Peer(Uid.predefined(b))))
      applyResult(b, nodes(b).addPeer(Peer(Uid.predefined(a))))
    }

    def removeLink(a: String, b: String): Unit = {
      adjacency.get(a).foreach(_ -= b)
      adjacency.get(b).foreach(_ -= a)
      nodes = nodes.updated(a, nodes(a).removePeer(Peer(Uid.predefined(b))))
      nodes = nodes.updated(b, nodes(b).removePeer(Peer(Uid.predefined(a))))
    }

    def removeNode(id: String): Unit = {
      adjacency.getOrElse(id, mutable.LinkedHashSet.empty).toList.foreach(n => removeLink(id, n))
      adjacency.remove(id)
      ids -= id
      nodes = nodes.removed(id)
      queue.filterInPlace(msg => msg.from != id && msg.to != id)
    }

    def broadcast(from: String, value: String): BroadcastRun = {
      val state = nodes(from)
      val dot = state.localContext.nextDot(Uid.predefined(from))
      val payload = Payload(Dots.single(dot), value)
      val deliveredBy = mutable.LinkedHashMap(from -> Option.empty[String])
      applyResult(from, state.broadcast(payload), Some(deliveredBy))
      drain(deliveredBy)
      BroadcastRun(value, deliveredBy.toMap)
    }

    def drain(): Unit = drain(mutable.LinkedHashMap.empty)

    private def drain(deliveredBy: mutable.LinkedHashMap[String, Option[String]]): Unit = {
      var safety = 0
      var idleRepairRounds = 0
      while idleRepairRounds < 2 && safety < 10000 do
        while queue.nonEmpty && safety < 10000 do
          val WireMessage(from, to, message) = queue.dequeue()
          safety += 1
          if ids.contains(from) && ids.contains(to) && adjacency.get(from).exists(_.contains(to)) then
            val result = nodes(to).handleMessage(Peer(Uid.predefined(from)), message)
            applyResult(to, result, Some(deliveredBy), from, message)
        val queueWasEmptyBeforeRepair = queue.isEmpty
        ids.foreach { id =>
          applyResult(id, nodes(id).repairTick(), Some(deliveredBy))
        }
        if queueWasEmptyBeforeRepair && queue.isEmpty then idleRepairRounds += 1
        else idleRepairRounds = 0
      assert(safety < 10000, s"message queue did not quiesce, remaining=${queue.size}")
    }

    private def applyResult(
                             owner: String,
                             result: PlumtreeBroadcast.Result[String],
                             deliveredBy: Option[mutable.LinkedHashMap[String, Option[String]]] = None,
                             incomingFrom: String = "",
                             incomingMessage: PlumtreeMessage[String] | Null = null,
    ): Unit = {
      nodes = nodes.updated(owner, result.state)
      if incomingMessage != null then incomingMessage match
        case payload: Payload[String] if result.events.exists {
              case Event.Deliver(`payload`) => true
              case _                        => false
            } =>
          deliveredBy.foreach { log =>
            if !log.contains(owner) then log.update(owner, Some(incomingFrom))
          }
        case _ => ()

      result.events.foreach {
        case Event.Deliver(_) => ()
        case Event.Disseminate(peers, message) =>
          peers.foreach { peer =>
            val to = Uid.unwrap(peer.uid)
            if ids.contains(to) && adjacency.get(owner).exists(_.contains(to)) then
              queue.enqueue(WireMessage(owner, to, message.asInstanceOf[PlumtreeMessage[String]]))
          }
      }
    }

    def currentEagerTree(active: Set[String] = ids.toSet): Set[Set[String]] =
      active.toList.flatMap { left =>
        active.toList.collect {
          case right if left < right &&
              nodes(left).peerRoles.get(Peer(Uid.predefined(right))).contains(PlumtreeBroadcast.PeerRole.Eager) &&
              nodes(right).peerRoles.get(Peer(Uid.predefined(left))).contains(PlumtreeBroadcast.PeerRole.Eager) => Set(left, right)
        }
      }.toSet
  }

  private case class BroadcastRun(value: String, deliveredBy: Map[String, Option[String]])

  private def assertTree(nodes: Set[String], edges: Set[Set[String]]): Unit = {
    assertEquals(edges.size, nodes.size - 1)

    val adjacency = edges.foldLeft(nodes.iterator.map(_ -> Set.empty[String]).toMap) { (acc, edge) =>
      val List(a, b) = edge.toList
      acc.updated(a, acc(a) + b).updated(b, acc(b) + a)
    }

    val start = nodes.head
    val visited = mutable.LinkedHashSet.empty[String]
    def dfs(current: String, parent: Option[String]): Unit =
      if !visited.contains(current) then
        visited += current
        adjacency(current).filterNot(parent.contains).foreach(next => dfs(next, Some(current)))

    dfs(start, None)
    assertEquals(visited.toSet, nodes)
  }

  test("plumtree builds a spanning eager tree after duplicate deliveries prune redundant edges") {
    val network = Network(List("a", "b", "c", "d", "e"))
    val ids = List("a", "b", "c", "d", "e")

    for
      i <- ids.indices
      j <- (i + 1) until ids.size
    do network.addLink(ids(i), ids(j))

    network.drain()
    val run = network.broadcast("a", "m1")

    assertEquals(run.deliveredBy.keySet, ids.toSet)
    assertEquals(run.deliveredBy("a"), None)
    assertEquals(run.deliveredBy.values.count(_.nonEmpty), ids.size - 1)

    val eagerTree = network.currentEagerTree()
    assertTree(ids.toSet, eagerTree)
  }

  test("plumtree expands the spanning tree when a new node joins") {
    val network = Network(List("a", "b", "c"))
    network.addLink("a", "b")
    network.addLink("a", "c")
    network.addLink("b", "c")
    network.drain()

    network.broadcast("a", "m1")

    network.addNode("d")
    network.addLink("b", "d")
    network.addLink("c", "d")
    network.drain()

    val run = network.broadcast("a", "m2")
    val active = Set("a", "b", "c", "d")

    assertEquals(run.deliveredBy.keySet, active)
    assert(run.deliveredBy("d").nonEmpty)

    val eagerTree = network.currentEagerTree(active)
    assertTree(active, eagerTree)
  }

  test("plumtree heals the tree through lazy links when an internal node leaves") {
    val network = Network(List("a", "b", "c", "d"))
    network.addLink("a", "b")
    network.addLink("a", "c")
    network.addLink("b", "d")
    network.addLink("c", "d")
    network.drain()

    val first = network.broadcast("a", "m1")
    assertEquals(first.deliveredBy.keySet, Set("a", "b", "c", "d"))
    assertEquals(first.deliveredBy("d"), Some("b"))

    network.removeNode("b")
    network.drain()

    val second = network.broadcast("a", "m2")
    val remaining = Set("a", "c", "d")

    assertEquals(second.deliveredBy.keySet, remaining)
    assertEquals(second.deliveredBy("d"), Some("c"))

    val eagerTree = network.currentEagerTree(remaining)
    assertTree(remaining, eagerTree)
  }
}
