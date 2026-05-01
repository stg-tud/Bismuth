package ex2026overlaydemo

import channels.{ConnectionDetails, LocalConnectionRegistry, LocalMessageQueue}
import replication.overlay.HyParViewMultiplexed
import replication.research.OverlayConnectionDirectory
import replication.research.OverlayNetworkProtocol.DemoState

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

  private def noReferencesTo(directory: OverlayConnectionDirectory.Directory, removed: Set[rdts.base.Uid]): Boolean =
    !directory.keySet.exists(removed.contains) &&
      directory.entries.forall((_, info) => info.peers.elements.forall(peer => !removed.contains(peer.uid)))

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

  test("living nodes eventually remove all references to stopped nodes from the replicated connection directory") {
    val fx    = QueuedFixture()
    val node1 = fx.newNode()
    fx.settle()
    val node2 = fx.newNode(seeds = List(node1.details))
    fx.settle()
    val node3 = fx.newNode(seeds = List(node2.details))
    fx.settle()
    val node4 = fx.newNode(seeds = List(node1.details))

    val living     = List(node1, node2)
    val removed    = List(node3, node4)
    val removedUids = removed.map(_.node.localUid.uid).toSet

    try {
      eventually({
        fx.drain()
        val expected = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid, node4.node.localUid.uid)
        living.forall(app => expected.subsetOf(app.node.connectionDirectory.keySet))
      }, rounds = 8000)

      fx.settle(20)
      node4.stop()
      fx.settle(20)
      node3.stop()

      eventually({
        fx.drain()
        living.forall { app =>
          val directory = app.node.connectionDirectory
          noReferencesTo(directory, removedUids)
        }
      }, rounds = 12000)
    } finally {
      try node4.stop() catch case _: Throwable => ()
      try node3.stop() catch case _: Throwable => ()
      node2.stop()
      node1.stop()
    }
  }
}
