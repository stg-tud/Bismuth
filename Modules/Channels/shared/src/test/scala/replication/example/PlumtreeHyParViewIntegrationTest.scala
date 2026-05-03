package replication.example

import channels.{ChannelConnectDescriptor, LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given}
import replication.overlay.HyParViewMultiplexed
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import replication.research.OverlayNetworkProtocol.DemoState

import scala.collection.mutable
import scala.util.Random

class PlumtreeHyParViewIntegrationTest extends munit.FunSuite {

  given codecString: JsonValueCodec[String]                                  = JsonCodecMaker.make
  given codecConnectionDetails: JsonValueCodec[ChannelConnectDescriptor]     = JsonCodecMaker.make
  given codecLinkState: JsonValueCodec[OverlayConnectionDirectory.LinkState] = JsonCodecMaker.make
  given codecPeerStates: JsonValueCodec[ObserveRemoveMap[Uid, OverlayConnectionDirectory.LinkState]] =
    ORMapStateCodec[Uid, OverlayConnectionDirectory.LinkState]
  given codecReplicatedSetUid: JsonValueCodec[ReplicatedSet[Uid]]          = AWSetStateCodec[Uid]
  given codecNodeInfo: JsonValueCodec[OverlayConnectionDirectory.NodeInfo] = JsonCodecMaker.make
  given codecReplicatedSetString: JsonValueCodec[ReplicatedSet[String]]    = AWSetStateCodec[String]
  given codecReplicatedSetConnectionDetails: JsonValueCodec[ReplicatedSet[ChannelConnectDescriptor]] =
    AWSetStateCodec[ChannelConnectDescriptor]
  given codecDirectoryState: JsonValueCodec[ObserveRemoveMap[Uid, OverlayConnectionDirectory.NodeInfo]] =
    ORMapStateCodec[Uid, OverlayConnectionDirectory.NodeInfo]
  given codecDemoState: JsonValueCodec[DemoState]                                      = JsonCodecMaker.make
  given codecOverlayEnvelope: JsonValueCodec[HyParViewMultiplexed.Envelope[DemoState]] = JsonCodecMaker.make

  type Envelope = HyParViewMultiplexed.Envelope[DemoState]

  private def drain(queue: LocalMessageQueue[Envelope], limit: Int): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
        queue.deliverAll()
        safety += 1
    assert(
      safety < limit,
      s"queue did not drain within $limit rounds, remaining=${queue.size}, messages=${queue.elements.take(10)}"
    )
  }

  private def tickOverlay(nodes: Iterable[OverlayDemoNode], queue: LocalMessageQueue[Envelope], rounds: Int): Unit =
    (0 until rounds).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      drain(queue, limit = 20000)
    }

  private def buildNetwork(n: Int)
      : (LocalMessageQueue[Envelope], Vector[(Uid, ChannelConnectDescriptor, OverlayDemoNode)]) = {
    val queue    = LocalMessageQueue[Envelope]()
    val queued   = mutable.LinkedHashMap.empty[String, QueuedLocalConnection[Envelope]]
    val registry = LocalConnectionRegistry[Envelope](queued)
    val random   = Random(7)
    val cfg      = HyParViewConfig.fromEstimatedNetworkSize(n)
    val resolver = registry

    val ids = Vector.tabulate(n)(i => Uid.predefined(s"hp$i"))

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val details = ChannelConnectDescriptor.QueuedLocal(Uid.unwrap(id))
      queued.update(Uid.unwrap(id), QueuedLocalConnection(queue))
      val listenEnvelope = registry.queuedServer(details).get
      val node           = new OverlayDemoNode(
        selfDetails = Set(details),
        listenEnvelope = Some(listenEnvelope),
        envelopeResolver = resolver,
        random = random,
        config = cfg,
        printOverlayEventsToStdout = false,
        runBackgroundTasks = false,
      )
      val seeds =
        if idx == 0 then Nil else List(ids.head).map(uid => ChannelConnectDescriptor.QueuedLocal(Uid.unwrap(uid)))
      node.start(seeds)
      drain(queue, limit = 20000)
      (node.localUid.uid, details, node)
    }

    (queue, nodes)
  }

  private def assertDirectoryKnows(directory: OverlayConnectionDirectory.Directory, expected: Set[Uid]): Unit =
    assertEquals(directory.entries.map(_._1).toSet, expected)

  private def assertViewsAreDisjoint(nodes: Iterable[OverlayDemoNode]): Unit =
    nodes.foreach { node =>
      val overlap = node.activeView intersect node.passiveView
      assertEquals(overlap, Set.empty, s"${Uid.unwrap(node.localUid.uid)} keeps peers in both active and passive views")
    }

  test("multiplexed hyparview keeps active and passive views disjoint during stabilization") {
    val (queue, nodes) = buildNetwork(12)
    val peers          = nodes.map(_._3)

    try {
      (0 until 10).foreach { _ =>
        peers.foreach(_.shuffleTick())
        drain(queue, limit = 200)
        assertViewsAreDisjoint(peers)
      }
    } finally peers.foreach(_.stop())
  }

  test("multiplexed hyparview handles join and leave while disseminating the connection directory") {
    val (queue, nodes) = buildNetwork(10)
    val peers          = nodes.map(_._3)

    try {
      tickOverlay(peers, queue, rounds = 8)

      val allNodeIds = nodes.map(_._1).toSet
      peers.foreach(node => assertDirectoryKnows(node.connectionDirectory, allNodeIds))
      assert(peers.forall(_.activeView.nonEmpty), "expected every node to have at least one active neighbor after join")

      val leaving   = nodes(4)._3
      val leavingId = nodes(4)._1
      val survivors = nodes.filterNot(_._1 == leavingId).map(_._3)

      leaving.stop()
      drain(queue, limit = 20000)
      tickOverlay(survivors, queue, rounds = 8)

      survivors.foreach { node =>
        assert(
          !node.activeView.contains(leavingId),
          s"${Uid.unwrap(node.localUid.uid)} still keeps departed node in active view"
        )
        assert(
          !node.passiveView.contains(leavingId),
          s"${Uid.unwrap(node.localUid.uid)} still keeps departed node in passive view"
        )
        assert(
          !node.connectionDirectory.get(node.localUid.uid).exists(_.value.peers.contains(leavingId)),
          s"${Uid.unwrap(node.localUid.uid)} still reports departed node in replicated local view"
        )
        assert(node.activeView.nonEmpty, s"${Uid.unwrap(node.localUid.uid)} lost all active neighbors after leave")
      }
    } finally peers.foreach(_.stop())
  }

  test("multiplexed hyparview heals after the bootstrap node leaves") {
    val (queue, nodes) = buildNetwork(10)
    val peers          = nodes.map(_._3)

    try {
      tickOverlay(peers, queue, rounds = 8)
      assertViewsAreDisjoint(peers)

      val leaving   = nodes.head._3
      val leavingId = nodes.head._1
      val survivors = nodes.tail.map(_._3)

      leaving.stop()
      drain(queue, limit = 20000)
      tickOverlay(survivors, queue, rounds = 10)

      survivors.foreach { node =>
        assert(
          !node.activeView.contains(leavingId),
          s"${Uid.unwrap(node.localUid.uid)} still keeps bootstrap node in active view"
        )
        assert(
          !node.passiveView.contains(leavingId),
          s"${Uid.unwrap(node.localUid.uid)} still keeps bootstrap node in passive view"
        )
        assert(
          node.activeView.nonEmpty,
          s"${Uid.unwrap(node.localUid.uid)} lost all active neighbors after bootstrap leave"
        )
      }
      assertViewsAreDisjoint(survivors)
    } finally peers.foreach(_.stop())
  }
}
