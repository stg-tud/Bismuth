package replication.example

import channels.{ConnectionDetails, ConnectionDetailsResolver, LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given}
import replication.overlay.HyParViewMultiplexed
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import replication.research.OverlayNetworkProtocol.DemoState

import scala.util.Random

class PlumtreeHyParViewIntegrationTest extends munit.FunSuite {

  given codecString: JsonValueCodec[String] = JsonCodecMaker.make
  given codecConnectionDetails: JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given codecLinkState: JsonValueCodec[OverlayConnectionDirectory.LinkState] = JsonCodecMaker.make
  given codecConnectedPeer: JsonValueCodec[OverlayConnectionDirectory.ConnectedPeer] = JsonCodecMaker.make
  given codecNodeInfo: JsonValueCodec[OverlayConnectionDirectory.NodeInfo] = JsonCodecMaker.make
  given codecReplicatedSetString: JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]
  given codecReplicatedSetConnectionDetails: JsonValueCodec[ReplicatedSet[ConnectionDetails]] = AWSetStateCodec[ConnectionDetails]
  given codecReplicatedSetConnectedPeer: JsonValueCodec[ReplicatedSet[OverlayConnectionDirectory.ConnectedPeer]] =
    AWSetStateCodec[OverlayConnectionDirectory.ConnectedPeer]
  given codecDirectoryState: JsonValueCodec[ObserveRemoveMap[Uid, OverlayConnectionDirectory.NodeInfo]] =
    ORMapStateCodec[Uid, OverlayConnectionDirectory.NodeInfo]
  given codecDemoState: JsonValueCodec[DemoState] = JsonCodecMaker.make
  given codecOverlayEnvelope: JsonValueCodec[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]] =
    HyParViewMultiplexed.envelopeCodec[DemoState, Set[ConnectionDetails]]

  type Envelope = HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]

  private def drain(queue: LocalMessageQueue[Envelope], limit: Int): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
      queue.deliverAll()
      safety += 1
    assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
  }

  private def tickOverlay(nodes: Iterable[OverlayDemoNode], queue: LocalMessageQueue[Envelope], rounds: Int): Unit =
    (0 until rounds).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      drain(queue, limit = 20000)
    }

  private def buildNetwork(n: Int): (LocalMessageQueue[Envelope], Vector[(Uid, ConnectionDetails, OverlayDemoNode)]) = {
    val queue    = LocalMessageQueue[Envelope]()
    val registry = LocalConnectionRegistry[Envelope]()
    val random   = Random(7)
    val cfg      = HyParViewConfig.fromEstimatedNetworkSize(n)
    val resolver = ConnectionDetailsResolver.many(registry)

    val ids = Vector.tabulate(n)(i => Uid.predefined(s"hp$i"))

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val details        = registry.registerQueued(Uid.unwrap(id), QueuedLocalConnection(queue))
      val listenEnvelope = registry.queuedServer(details).get
      val node = new OverlayDemoNode(
        selfDetails = Set(details),
        listenEnvelope = Some(listenEnvelope),
        envelopeResolver = resolver,
        random = random,
        config = cfg,
        printOverlayEventsToStdout = false,
      )
      val seeds = if idx == 0 then Nil else List(ids.head).map(uid => ConnectionDetails.QueuedLocal(Uid.unwrap(uid)))
      node.start(seeds)
      drain(queue, limit = 20000)
      (node.localUid.uid, details, node)
    }

    (queue, nodes)
  }

  private def assertDirectoryKnows(directory: OverlayConnectionDirectory.Directory, expected: Set[Uid]): Unit =
    assertEquals(directory.entries.map(_._1).toSet, expected)

  test("multiplexed hyparview handles join and leave while disseminating the connection directory") {
    val (queue, nodes) = buildNetwork(10)
    val peers          = nodes.map(_._3)

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
      assert(!node.activeView.contains(leavingId), s"${Uid.unwrap(node.localUid.uid)} still keeps departed node in active view")
      assert(!node.passiveView.contains(leavingId), s"${Uid.unwrap(node.localUid.uid)} still keeps departed node in passive view")
      assert(
        !node.connectionDirectory.get(node.localUid.uid).exists(_.peers.elements.exists(_.uid == leavingId)),
        s"${Uid.unwrap(node.localUid.uid)} still reports departed node in replicated local view"
      )
      assert(node.activeView.nonEmpty, s"${Uid.unwrap(node.localUid.uid)} lost all active neighbors after leave")
    }
  }
}
