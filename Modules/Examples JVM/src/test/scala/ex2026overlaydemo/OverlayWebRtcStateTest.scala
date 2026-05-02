package ex2026overlaydemo

import channels.{LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import replication.overlay.HyParViewMultiplexed
import replication.research.OverlayNetworkProtocol.DemoState

import scala.collection.mutable
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayWebRtcStateTest extends munit.FunSuite {

  private def drain(queue: LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState]], limit: Int = 20000): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
      queue.deliverAll()
      safety += 1
    assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
  }

  test("queued overlay demo nodes can still join without webrtc support") {
    val queue    = LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState]]()
    val queued   = mutable.LinkedHashMap("n1" -> QueuedLocalConnection(queue), "n2" -> QueuedLocalConnection(queue))
    val registry = LocalConnectionRegistry[HyParViewMultiplexed.Envelope[DemoState]](queued)

    val node1 = new OverlayDemo.NodeApp(OverlayDemo.TopicNode.queued(registry, "n1", Random(1)))
    drain(queue)
    val node2 = new OverlayDemo.NodeApp(OverlayDemo.TopicNode.queued(registry, "n2", Random(2)), seeds = List(node1.details))
    drain(queue)
    drain(queue)
    drain(queue)

    try {
      node1.node.shuffleTick()
      node2.node.shuffleTick()
      drain(queue)
      assert(node1.node.activeView.nonEmpty)
      assert(node2.node.activeView.nonEmpty)
    } finally {
      node2.stop()
      node1.stop()
    }
  }
}
