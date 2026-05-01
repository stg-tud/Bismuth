package ex2026overlaydemo

import channels.{ConnectionDetails, LocalConnectionRegistry, LocalMessageQueue}
import replication.overlay.HyParViewMultiplexed
import replication.research.OverlayNetworkProtocol.{DemoState, WebRtcOffer}

import scala.util.Random

/** vibecoded. dont trust 😉 */
class OverlayWebRtcStateTest extends munit.FunSuite {

  private def drain(queue: LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]], limit: Int = 20000): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
      queue.deliverAll()
      safety += 1
    assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
  }

  test("webrtc offers replicate through overlay demo state") {
    val queue    = LocalMessageQueue[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]()
    val registry = LocalConnectionRegistry[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]()

    val node1 = new OverlayDemo.NodeApp(OverlayDemo.TopicNode.queued(registry, "n1", queue, Random(1)))
    drain(queue)
    val node2 = new OverlayDemo.NodeApp(OverlayDemo.TopicNode.queued(registry, "n2", queue, Random(2)), seeds = List(node1.details))
    drain(queue)
    drain(queue)
    drain(queue)

    try {
      val offer = WebRtcOffer("offer-1", "offer", "dummy-sdp")
      node1.node.asInstanceOf[OverlayDemo.TopicNode].core.publishWebRtcOffer(node2.node.localUid.uid, offer)
      drain(queue)
      drain(queue)
      drain(queue)

      assert(node1.node.state.webRtcOffers.get(node2.node.localUid.uid).flatMap(_.get(node1.node.localUid.uid)).flatMap(_.read).contains(offer))
      assert(node2.node.state.webRtcOffers.get(node2.node.localUid.uid).flatMap(_.get(node1.node.localUid.uid)).flatMap(_.read).contains(offer))
    } finally {
      node2.stop()
      node1.stop()
    }
  }
}
