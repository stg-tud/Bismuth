package replication

import channels.{ConnectionDescriptor, LocalConnectionRegistry, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import munit.FunSuite
import rdts.base.Lattice.syntax.merge
import rdts.base.LocalUid
import replication.overlay.FullMeshOverlay
import replication.overlay.HyParViewStateMachine
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.research.OverlayStatusProtocol
import replication.research.OverlayStatusProtocol.{PeerState, Status}

import scala.util.Random

class OverlayStatusProtocolTest extends FunSuite {

  final case class Node(id: String) {
    val uid: LocalUid           = LocalUid.gen()
    val selfInfo                = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
    var status: Status          = OverlayStatusProtocol.empty
    val io: BroadcastIO[Status] = BroadcastIO[Status](
      uid,
      delta => status = status.merge(delta),
      overlay = Some(FullMeshOverlay(selfInfo)),
    )

    def publishStatus(round: Long): Unit = {
      given LocalUid = uid
      val delta      = OverlayStatusProtocol.statusDelta(status, io, timestamp = round)
      status = status.merge(delta)
      io.broadcast(delta)
    }
  }

  private def connect(queue: LocalMessageQueue, left: Node, right: Node, label: String): Unit = {
    val link = QueuedLocalConnection(label, queue)
    left.io.addServerConnection(link.server)
    right.io.addClientConnection(link.client(label))
  }

  private def drain(queue: LocalMessageQueue, limit: Int = 1000): Unit = {
    var rounds = 0
    while queue.nonEmpty && rounds < limit do
        queue.deliverAll()
        rounds += 1
    assert(rounds < limit, s"queue did not quiesce, remaining=${queue.size}")
  }

  test("status RDT reflects a manually connected direct overlay and converges on all peers") {
    val queue = LocalMessageQueue()
    val n0    = Node("n0")
    val n1    = Node("n1")
    val n2    = Node("n2")
    val n3    = Node("n3")
    val nodes = Vector(n0, n1, n2, n3)

    connect(queue, n0, n1, "0-1")
    connect(queue, n1, n2, "1-2")
    connect(queue, n2, n3, "2-3")
    drain(queue)
    nodes.foreach(_.io.tick())
    drain(queue)

    (1L to 6L).foreach { round =>
      nodes.foreach(_.publishStatus(round))
      drain(queue)
      nodes.foreach(_.io.tick())
      drain(queue)
    }

    val expected = Map(
      n0.uid.uid -> Map(n1.uid.uid -> PeerState.Eager),
      n1.uid.uid -> Map(n0.uid.uid -> PeerState.Eager, n2.uid.uid -> PeerState.Eager),
      n2.uid.uid -> Map(n1.uid.uid -> PeerState.Eager, n3.uid.uid -> PeerState.Eager),
      n3.uid.uid -> Map(n2.uid.uid -> PeerState.Eager),
    )

    val snapshots = nodes.map(node => OverlayStatusProtocol.snapshot(node.status))

    snapshots.foreach(snapshot => assertEquals(snapshot, expected))
    snapshots.tail.foreach(snapshot => assertEquals(snapshot, snapshots.head))
  }

  test("status RDT converges across peers when using HyParView and includes passive knowledge") {
    val nodeCount = 6
    val queue     = LocalMessageQueue()
    val links     = (0 until nodeCount).map(i => s"h$i" -> QueuedLocalConnection(s"h$i", queue)).toMap
    val resolver  = LocalConnectionRegistry(links)
    val config    = HyParViewConfig(
      activeViewSize = 2,
      passiveViewSize = 8,
      activeRandomWalkLength = 3,
      passiveRandomWalkLength = 1,
      shuffleRandomWalkLength = 2,
      shuffleActiveSample = 1,
      shufflePassiveSample = 2,
    )

    final case class HyparNode(id: String, random: Random) {
      val uid: LocalUid           = LocalUid.gen()
      val selfInfo                = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      var status: Status          = OverlayStatusProtocol.empty
      val io: BroadcastIO[Status] = BroadcastIO[Status](
        uid,
        delta => status = status.merge(delta),
        overlay = Some(HyParViewStateMachine.empty(selfInfo, config, random.between, _ => true)),
        resolver = resolver,
      )

      def startListening(): Unit =
        io.addServerConnection(resolver.queuedServer(selfInfo.channelConnectors.head).get)

      def bootstrapVia(peer: ConnectionDescriptor): Unit =
        io.bootstrapVia(peer)

      def publishStatus(round: Long): Unit = {
        given LocalUid = uid
        val delta      = OverlayStatusProtocol.statusDelta(status, io, timestamp = round)
        status = status.merge(delta)
        io.broadcast(delta)
      }

      def activeView: Set[rdts.base.Uid] =
        io.overlayController.asInstanceOf[HyParViewStateMachine].activeView

      def passiveView: Set[rdts.base.Uid] =
        io.overlayController.asInstanceOf[HyParViewStateMachine].passiveView
    }

    val nodes = Vector.tabulate(nodeCount)(i => HyparNode(s"h$i", Random(0xb15 + i)))
    nodes.foreach(_.startListening())

    nodes.indices.foreach { i =>
      Vector(i - 1, i + 1)
        .filter(j => j >= 0 && j < nodes.size)
        .map(nodes(_).selfInfo.channelConnectors.head)
        .foreach(nodes(i).bootstrapVia)
    }

    def settle(rounds: Int): Unit =
      (0 until rounds).foreach { round =>
        drain(queue, limit = 4000)
        nodes.foreach(_.io.tick())
        nodes.foreach(_.publishStatus(round.toLong + 1L))
      }

    settle(80)
    drain(queue, limit = 4000)
    nodes.foreach(_.publishStatus(999L))
    drain(queue, limit = 4000)

    val snapshots = nodes.map(node => OverlayStatusProtocol.snapshot(node.status))
    val allUids   = nodes.map(_.uid.uid).toSet

    def normalized(snapshot: Map[rdts.base.Uid, Map[rdts.base.Uid, PeerState]]) =
      snapshot.view.mapValues(_.view.mapValues {
        case PeerState.Passive => PeerState.Passive
        case PeerState.Lazy    => PeerState.Eager
        case PeerState.Eager   => PeerState.Eager
      }.toMap).toMap

    snapshots.foreach(snapshot => assertEquals(snapshot.keySet, allUids))
    val normalizedSnapshots = snapshots.map(normalized)
    normalizedSnapshots.tail.foreach(snapshot => assertEquals(snapshot, normalizedSnapshots.head))

    val allPeerStates = snapshots.head.valuesIterator.flatMap(_.valuesIterator).toSet
    assert(allPeerStates.contains(PeerState.Passive), s"expected passive edges in snapshot, got $allPeerStates")
    assert(
      nodes.forall(_.activeView.nonEmpty),
      s"expected all nodes to have active peers, got ${nodes.map(_.activeView.size)}"
    )
  }
}
