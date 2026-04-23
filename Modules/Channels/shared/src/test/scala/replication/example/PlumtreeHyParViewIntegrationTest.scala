package replication.example

import channels.{ConnectionDetails, LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{LocalUid, Uid}
import replication.PlumtreeDissemination
import replication.overlay.{HyParViewMultiplexed, HyParViewMultiplexedNode, HyParViewUnified}

import scala.util.Random

class PlumtreeHyParViewIntegrationTest extends munit.FunSuite {

  given JsonValueCodec[String] = JsonCodecMaker.make
  given JsonValueCodec[Set[String]] = JsonCodecMaker.make
  given JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given JsonValueCodec[HyParViewMultiplexed.Envelope[Set[String], ConnectionDetails]] =
    HyParViewMultiplexed.envelopeCodec[Set[String], ConnectionDetails]

  private def drain(queue: LocalMessageQueue[HyParViewMultiplexed.Envelope[Set[String], ConnectionDetails]], limit: Int): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
      queue.deliverAll()
      safety += 1
    assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
  }

  private def runNetwork(n: Int): Unit = {
    val queue    = LocalMessageQueue[HyParViewMultiplexed.Envelope[Set[String], ConnectionDetails]]()
    val registry = LocalConnectionRegistry[HyParViewMultiplexed.Envelope[Set[String], ConnectionDetails]]()
    val random   = Random(7)

    val ids = Vector.tabulate(n)(i => Uid.predefined(s"hp$i"))
    val dms = ids.map(_ => PlumtreeDissemination[Set[String]](LocalUid.gen(), _ => (), None))
    val cfg = HyParViewUnified.HyParViewConfig.fromEstimatedNetworkSize(n)

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val details = registry.registerQueued(Uid.unwrap(id), QueuedLocalConnection(queue))
      HyParViewMultiplexedNode(
        HyParViewMultiplexed.PeerRef(id, details),
        dms(idx),
        registry.queuedServer(details).get,
        registry,
        if idx == 0 then None else Some(ids.head).map(h => ConnectionDetails.QueuedLocal(Uid.unwrap(h))),
        random,
        cfg
      )
    }

    nodes.foreach(_.startServer())
    nodes.head.join()
    nodes.tail.foreach { node =>
      node.join()
      drain(queue, limit = 20000)
    }

    (0 until 5).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      drain(queue, limit = 20000)
    }

    dms.zipWithIndex.foreach { case (dm, i) => dm.applyDelta(Set(s"x$i")) }
    drain(queue, limit = 50000)

    val expected = ids.indices.map(i => Set(s"x$i")).toSet
    dms.foreach { dm =>
      assertEquals(dm.allPayloads.map(_.payload.data).toSet, expected)
    }
    assert(nodes.count(_.activeView.nonEmpty) >= math.min(2, n))
  }

  test("multiplexed hyparview drives plumtree on a 10 node network") {
    runNetwork(10)
  }

  test("multiplexed hyparview drives plumtree on a 100 node network") {
    runNetwork(100)
  }
}
