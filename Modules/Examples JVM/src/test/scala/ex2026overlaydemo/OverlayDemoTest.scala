package ex2026overlaydemo

import channels.{ConnectionDetails, LocalConnectionRegistry, LocalMessageQueue}
import replication.overlay.HyParViewMultiplexed
import replication.research.OverlayConnectionDirectory
import replication.research.OverlayConnectionDirectory.LinkState
import replication.research.OverlayNetworkProtocol.DemoState
import rdts.base.Uid

import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayDemoTest extends munit.FunSuite {

  private val queueLimit = 20000

  private def eventually(cond: => Boolean, rounds: Int = 2000): Unit = {
    var remaining = rounds
    while remaining > 0 && !cond do
      remaining -= 1
    assert(cond)
  }

  private def localInfoMatchesNode(app: OverlayDemo.NodeApp): Boolean = {
    val localUid = app.node.localUid.uid
    app.node.connectionDirectory.get(localUid).exists { info =>
      val replicatedActive = info.peers.elements.collect { case OverlayConnectionDirectory.ConnectedPeer(uid, LinkState.Active) => uid }
      val replicatedPassive = info.peers.elements.collect { case OverlayConnectionDirectory.ConnectedPeer(uid, LinkState.Passive) => uid }
      val replicatedEager = info.eagerPeers.elements
      replicatedActive == app.node.activeView &&
      replicatedPassive == app.node.passiveView &&
      replicatedEager == app.node.eagerView &&
      replicatedEager.subsetOf(replicatedActive)
    }
  }

  private def eagerEdgesFormForest(directory: OverlayConnectionDirectory.Directory): Boolean = {
    val eagerByNode = directory.entries.iterator.map { (uid, info) =>
      val activePeers = info.peers.elements.collect { case OverlayConnectionDirectory.ConnectedPeer(peerUid, LinkState.Active) => peerUid }
      uid -> (activePeers intersect info.eagerPeers.elements)
    }.toMap

    val undirectedEdges = eagerByNode.iterator.flatMap { (left, peers) =>
      peers.iterator.collect {
        case right if eagerByNode.getOrElse(right, Set.empty).contains(left) =>
          if Uid.unwrap(left) <= Uid.unwrap(right) then (left, right) else (right, left)
      }
    }.toSet

    val adjacency = undirectedEdges.foldLeft(Map.empty[Uid, Set[Uid]]) { case (acc, (left, right)) =>
      acc
        .updated(left, acc.getOrElse(left, Set.empty) + right)
        .updated(right, acc.getOrElse(right, Set.empty) + left)
    }

    def dfs(current: Uid, parent: Option[Uid], visited: Set[Uid]): (Boolean, Set[Uid]) = {
      val nextVisited = visited + current
      adjacency.getOrElse(current, Set.empty).foldLeft((true, nextVisited)) {
        case ((false, seen), _) => (false, seen)
        case ((true, seen), neighbor) if parent.contains(neighbor) => (true, seen)
        case ((true, seen), neighbor) if seen.contains(neighbor) => (false, seen)
        case ((true, seen), neighbor) => dfs(neighbor, Some(current), seen)
      }
    }

    adjacency.keys.foldLeft((true, Set.empty[Uid])) {
      case ((false, visited), _) => (false, visited)
      case ((true, visited), uid) if visited.contains(uid) => (true, visited)
      case ((true, visited), uid) => dfs(uid, None, visited)
    }._1
  }

  private class QueuedFixture {
    val queue    = LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]]()
    val registry = LocalConnectionRegistry[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]]()
    private var nextId = 0

    def newNode(seeds: List[ConnectionDetails] = Nil): OverlayDemo.NodeApp = {
      val id   = s"queued-$nextId"
      nextId += 1
      new OverlayDemo.NodeApp(
        OverlayDemo.TopicNode.queued(
          registry = registry,
          id = id,
          queue = queue,
          random = Random(nextId),
        ),
        seeds = seeds,
      )
    }

    def drain(limit: Int = queueLimit): Unit = {
      var safety = 0
      while queue.nonEmpty && safety < limit do
        queue.deliverAll()
        safety += 1
      assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
    }

    def settle(rounds: Int = 6): Unit =
      var i = 0
      while i < rounds do
        drain()
        i += 1
      drain()
  }

  test("nodes replicate self connection info and payload state through plumtree") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    fx.settle()
    val node2 = fx.newNode(seeds = List(node1.details))

    try {
      fx.settle(12)

      eventually({
        fx.drain()
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty
      })

      eventually({
        fx.drain()
        val directory1 = node1.node.connectionDirectory
        val directory2 = node2.node.connectionDirectory
        Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory1.keySet) &&
        Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory2.keySet)
      })

      node1.node.publishAdd("server-value")
      node2.node.publishAdd("client-value")

      eventually({
        fx.drain()
        val expected = Set("server-value", "client-value")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected
      })

      node2.node.publishRemove("server-value")

      eventually({
        fx.drain()
        val expected = Set("client-value")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected
      })
    } finally {
      node2.stop()
      node1.stop()
    }
  }

  test("nodes learn the whole network from replicated local views and can use that for further overlay connectivity") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    fx.settle()
    val node2 = fx.newNode(seeds = List(node1.details))
    fx.settle()
    val node3 = fx.newNode(seeds = List(node2.details))

    try {
      fx.settle(20)

      eventually({
        fx.drain()
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty && node3.node.activeView.nonEmpty
      })

      eventually({
        fx.drain()
        val expected = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
        val directories = List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
        directories.forall(directory => expected.subsetOf(directory.keySet))
      }, rounds = 5000)

      eventually({
        fx.drain()
        val allNodes = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
        List(node1, node2, node3).forall { app =>
          val knownActiveOrPassive = app.node.activeView ++ app.node.passiveView + app.node.localUid.uid
          allNodes.subsetOf(knownActiveOrPassive)
        }
      }, rounds = 8000)

      node1.handleInputLine("alpha")
      node2.handleInputLine("beta")
      assert(node3.handleInputLine("gamma"))
      assert(!node3.handleInputLine("q"))

      eventually({
        fx.drain()
        val expected = Set("alpha", "beta", "gamma")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected &&
        node3.node.state.values.elements == expected
      })

      val directories = List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
      assert(directories.exists(_.entries.exists((_, info) => info.peers.elements.exists(_.state == OverlayConnectionDirectory.LinkState.Active))))
    } finally {
      node3.stop()
      node2.stop()
      node1.stop()
    }
  }

  test("replicated local active passive and eager info matches node state and eager edges form a forest") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    fx.settle()
    val node2 = fx.newNode(seeds = List(node1.details))
    fx.settle()
    val node3 = fx.newNode(seeds = List(node1.details))

    try {
      eventually({
        fx.settle(20)
        List(node1, node2, node3).forall(app => app.node.activeView.nonEmpty)
      }, rounds = 8000)

      node1.node.publishAdd("alpha")
      node2.node.publishAdd("beta")
      node3.node.publishAdd("gamma")

      eventually({
        fx.settle(30)
        val expected = Set("alpha", "beta", "gamma")
        List(node1, node2, node3).forall(_.node.state.values.elements == expected)
      }, rounds = 12000)

      eventually({
        fx.settle(20)
        val apps = List(node1, node2, node3)
        apps.forall(localInfoMatchesNode)
      }, rounds = 12000)

    } finally {
      node3.stop()
      node2.stop()
      node1.stop()
    }
  }

  test("replicated eager edges form a tree on a two node network") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    fx.settle()
    val node2 = fx.newNode(seeds = List(node1.details))

    try {
      eventually({
        fx.settle(20)
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty
      }, rounds = 8000)

      node1.node.publishAdd("alpha")
      node2.node.publishAdd("beta")

      eventually({
        fx.settle(30)
        val expected = Set("alpha", "beta")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected &&
        localInfoMatchesNode(node1) &&
        localInfoMatchesNode(node2) &&
        eagerEdgesFormForest(node1.node.connectionDirectory)
      }, rounds = 12000)
    } finally {
      node2.stop()
      node1.stop()
    }
  }

}
