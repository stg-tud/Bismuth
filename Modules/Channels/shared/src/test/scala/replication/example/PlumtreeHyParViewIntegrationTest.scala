package replication.example

import channels.{ConnectionDetails, LocalConnectionRegistry, LocalMessageQueue, QueuedLocalConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.base.Lattice.syntax
import replication.JsoniterCodecs
import replication.JsoniterCodecs.given
import replication.PlumtreeDissemination
import replication.overlay.{HyParViewMultiplexed, HyParViewMultiplexedNode, HyParViewUnified}
import replication.research.OverlayConnectionDirectory
import replication.research.OverlayConnectionDirectory.{ConnectedPeer, LinkState, NodeInfo}

import scala.util.Random

class PlumtreeHyParViewIntegrationTest extends munit.FunSuite {

  type Directory = OverlayConnectionDirectory.Directory[ConnectionDetails]
  type Envelope  = HyParViewMultiplexed.Envelope[Directory, ConnectionDetails]

  given JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given JsonValueCodec[LinkState] = JsonCodecMaker.make
  given JsonValueCodec[ConnectedPeer[ConnectionDetails]] = JsonCodecMaker.make
  given JsonValueCodec[NodeInfo[ConnectionDetails]] = JsonCodecMaker.make
  given JsonValueCodec[Directory] = JsoniterCodecs.ORMapStateCodec[Uid, NodeInfo[ConnectionDetails]]
  given JsonValueCodec[Envelope] = HyParViewMultiplexed.envelopeCodec[Directory, ConnectionDetails]

  private final class StateRef(var value: Directory)

  private case class TestNode(
      id: Uid,
      localUid: LocalUid,
      details: ConnectionDetails,
      dissemination: PlumtreeDissemination[Directory],
      overlay: HyParViewMultiplexedNode[Directory, ConnectionDetails],
      stateRef: StateRef,
  ) {
    def state: Directory = stateRef.value

    def publishDirectoryDelta(delta: Directory): Unit =
      if !Bottom.isEmpty(delta) then
        stateRef.value = stateRef.value.merge(delta)
        dissemination.applyDelta(delta)

    def publishCurrentView(): Unit = {
      given LocalUid = localUid
      publishDirectoryDelta(
        OverlayConnectionDirectory.updateNodeFromOverlay(
          state,
          id,
          Set(details),
          overlay.activePeers,
          overlay.passivePeers,
        )
      )
    }

    def publishLeaveDelta(): Unit = {
      given LocalUid = localUid
      publishDirectoryDelta(OverlayConnectionDirectory.removeNodeEverywhere(state, id))
    }

    def startAndMaybeJoin(): Unit = {
      overlay.startServer()
      publishCurrentView()
      overlay.join()
    }

    def stopGracefully(): Unit =
      overlay.stop(graceful = true)
  }

  private def drain(queue: LocalMessageQueue[Envelope], limit: Int): Unit = {
    var safety = 0
    while queue.nonEmpty && safety < limit do
      queue.deliverAll()
      safety += 1
    assert(safety < limit, s"queue did not drain within $limit rounds, remaining=${queue.size}")
  }

  private def tickOverlay(nodes: Iterable[TestNode], queue: LocalMessageQueue[Envelope], rounds: Int): Unit =
    (0 until rounds).foreach { _ =>
      nodes.foreach(_.overlay.shuffleTick())
      drain(queue, limit = 20000)
    }

  private def buildNetwork(n: Int): (LocalMessageQueue[Envelope], Vector[TestNode]) = {
    val queue    = LocalMessageQueue[Envelope]()
    val registry = LocalConnectionRegistry[Envelope]()
    val random   = Random(7)
    val cfg      = HyParViewUnified.HyParViewConfig.fromEstimatedNetworkSize(n)

    val ids = Vector.tabulate(n)(i => Uid.predefined(s"hp$i"))

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val localUid = LocalUid.gen()
      val stateRef = StateRef(OverlayConnectionDirectory.empty[ConnectionDetails])

      val dissemination = PlumtreeDissemination[Directory](
        localUid,
        delta => stateRef.value = stateRef.value.merge(delta),
        None,
      )

      val details = registry.registerQueued(Uid.unwrap(id), QueuedLocalConnection(queue))

      lazy val node: TestNode = TestNode(
        id,
        localUid,
        details,
        dissemination,
        HyParViewMultiplexedNode(
          HyParViewMultiplexed.PeerRef(id, details),
          dissemination,
          registry.queuedServer(details).get,
          registry,
          if idx == 0 then None else Some(ConnectionDetails.QueuedLocal(Uid.unwrap(ids.head))),
          random,
          cfg,
          onViewChanged = (_, _) => node.publishCurrentView(),
          onPeerDisconnected = peer => {
            given LocalUid = localUid
            node.publishDirectoryDelta(OverlayConnectionDirectory.removeNodeEverywhere(node.state, peer))
          },
        ),
        stateRef,
      )

      node
    }

    (queue, nodes)
  }

  private def startNetwork(nodes: Vector[TestNode], queue: LocalMessageQueue[Envelope]): Unit = {
    nodes.head.startAndMaybeJoin()
    drain(queue, limit = 20000)

    nodes.tail.foreach { node =>
      node.startAndMaybeJoin()
      drain(queue, limit = 20000)
    }

    tickOverlay(nodes, queue, rounds = 8)
  }

  private def assertDirectoryKnows(directory: Directory, expected: Set[Uid]): Unit = {
    assertEquals(directory.entries.map(_._1).toSet, expected)
  }

  test("multiplexed hyparview handles join and leave while disseminating the connection directory") {
    val (queue, nodes) = buildNetwork(10)
    startNetwork(nodes, queue)

    val allNodeIds = nodes.map(_.id).toSet

    nodes.foreach(node => assertDirectoryKnows(node.state, allNodeIds))
    assert(nodes.forall(_.overlay.activeView.nonEmpty), "expected every node to have at least one active neighbor after join")

    val leaving   = nodes(4)
    val survivors = nodes.filterNot(_.id == leaving.id)

    leaving.publishLeaveDelta()
    drain(queue, limit = 20000)
    leaving.stopGracefully()
    drain(queue, limit = 20000)

    tickOverlay(survivors, queue, rounds = 8)

    val survivorIds = survivors.map(_.id).toSet
    survivors.foreach { node =>
      assertDirectoryKnows(node.state, survivorIds)
      assert(!node.overlay.activeView.contains(leaving.id), s"${Uid.unwrap(node.id)} still keeps departed node in active view")
      assert(!node.overlay.passiveView.contains(leaving.id), s"${Uid.unwrap(node.id)} still keeps departed node in passive view")
      assert(node.overlay.activeView.nonEmpty, s"${Uid.unwrap(node.id)} lost all active neighbors after leave")
    }
  }
}
