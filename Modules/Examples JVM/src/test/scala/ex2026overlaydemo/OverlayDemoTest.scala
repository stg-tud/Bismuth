package ex2026overlaydemo

import channels.{ChannelConnectDescriptor, LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import rdts.base.Uid
import replication.overlay.HyParViewMultiplexed
import replication.research.OverlayConnectionDirectory
import replication.research.OverlayNetworkProtocol.DemoState

import scala.collection.mutable
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayDemoTest extends munit.FunSuite {

  private val queueLimit = 20

  private def localInfoMatchesNode(app: OverlayDemo.NodeApp): Boolean = {
    val localUid = app.node.localUid.uid
    app.node.connectionDirectory.get(localUid).exists { info =>
      val replicated = OverlayConnectionDirectory.snapshot(info)
      replicated.active == app.node.activeView &&
      replicated.passive == app.node.passiveView &&
      replicated.eager == app.node.eagerView &&
      replicated.eager.subsetOf(replicated.active)
    }
  }

  private def eagerEdgesFormForest(directory: OverlayConnectionDirectory.Directory): Boolean = {
    val eagerByNode = directory.entries.iterator.map { (uid, info) =>
      val snapshot = OverlayConnectionDirectory.snapshot(info)
      uid -> (snapshot.active intersect snapshot.eager)
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
        case ((false, seen), _)                                    => (false, seen)
        case ((true, seen), neighbor) if parent.contains(neighbor) => (true, seen)
        case ((true, seen), neighbor) if seen.contains(neighbor)   => (false, seen)
        case ((true, seen), neighbor)                              => dfs(neighbor, Some(current), seen)
      }
    }

    adjacency.keys.foldLeft((true, Set.empty[Uid])) {
      case ((false, visited), _)                           => (false, visited)
      case ((true, visited), uid) if visited.contains(uid) => (true, visited)
      case ((true, visited), uid)                          => dfs(uid, None, visited)
    }._1
  }

  private class QueuedFixture {
    val queue    = LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState]]()
    val queued   = mutable.LinkedHashMap.empty[String, QueuedLocalConnection[HyParViewMultiplexed.Envelope[DemoState]]]
    val registry = LocalConnectionRegistry[HyParViewMultiplexed.Envelope[DemoState]](queued)
    private val apps   = mutable.ArrayBuffer.empty[OverlayDemo.NodeApp]
    private var nextId = 0

    def newNode(seeds: List[ChannelConnectDescriptor] = Nil): OverlayDemo.NodeApp = {
      val id = s"queued-$nextId"
      nextId += 1
      queued.update(id, QueuedLocalConnection(queue))
      val app = new OverlayDemo.NodeApp(
        OverlayDemo.TopicNode.queued(
          registry = registry,
          id = id,
          random = Random(nextId),
        ),
        seeds = seeds,
      )
      apps += app
      app
    }

    def drain(): Unit = {
      var safety           = 0
      var idleRepairRounds = 0
      while idleRepairRounds < 2 && safety < queueLimit do
          queue.deliverAll()
          safety += 1
          val queueWasEmptyBeforeRepair = !queue.nonEmpty
          apps.foreach(_.node.repairTick())
          if queueWasEmptyBeforeRepair && !queue.nonEmpty then idleRepairRounds += 1
          else idleRepairRounds = 0
      assert(safety < queueLimit, s"queue did not drain within $queueLimit rounds, remaining=${queue.size}")
    }

    def settle(): Unit =
        drain()
        apps.foreach(_.node.shuffleTick())
        drain()
  }

  test("nodes replicate self connection info and payload state through plumtree") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    val node2 = fx.newNode(seeds = List(node1.details))

    try {

      fx.drain()
      assert {
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty
      }

      fx.drain()
      val directory1 = node1.node.connectionDirectory
      val directory2 = node2.node.connectionDirectory
      assert(Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory1.keySet))
      assert(Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory2.keySet))
    } finally {
      node2.stop()
      node1.stop()
    }
  }

  test("nodes learn the whole network from replicated local views and can use that for further overlay connectivity") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    val node2 = fx.newNode(seeds = List(node1.details))
    val node3 = fx.newNode(seeds = List(node2.details))

    try {
      fx.settle()

      assert {
        fx.drain()
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty && node3.node.activeView.nonEmpty
      }

      assert {
        fx.drain()
        val expected    = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
        val directories =
          List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
        directories.forall(directory => expected.subsetOf(directory.keySet))
      }

      fx.drain()
      val allNodes = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
      List(node1, node2, node3).foreach { app =>
        val knownActiveOrPassive = app.node.activeView ++ app.node.passiveView + app.node.localUid.uid
        assert(allNodes.subsetOf(knownActiveOrPassive))
      }

      val directories =
        List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
      assert(directories.exists(_.entries.exists((_, info) =>
        OverlayConnectionDirectory.snapshot(info).active.nonEmpty
      )))
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
      assert {
        fx.settle()
        List(node1, node2, node3).forall(app => app.node.activeView.nonEmpty)
      }

      assert {
        fx.settle()
        val apps = List(node1, node2, node3)
        apps.forall(localInfoMatchesNode)
      }

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
      fx.settle()
      assert(node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty)
      assert(localInfoMatchesNode(node1))
      assert(localInfoMatchesNode(node2))
      assert(eagerEdgesFormForest(node1.node.connectionDirectory))
    } finally {
      node2.stop()
      node1.stop()
    }
  }

}
