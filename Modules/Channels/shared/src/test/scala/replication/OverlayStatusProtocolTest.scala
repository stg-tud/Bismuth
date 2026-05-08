package replication

import channels.{ChannelConnectInfo, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import munit.FunSuite
import rdts.base.Lattice.syntax.merge
import rdts.base.LocalUid
import replication.overlay.DirectConnectionOverlay
import replication.research.OverlayStatusProtocol
import replication.research.OverlayStatusProtocol.{PeerState, Status}

class OverlayStatusProtocolTest extends FunSuite {

  final case class Node(id: String) {
    val uid: LocalUid = LocalUid.gen()
    val selfInfo      = PeerConnectInfo(uid.uid, Set(ChannelConnectInfo.QueuedLocal(id)))
    var status: Status = OverlayStatusProtocol.empty
    val io: BroadcastIO[Status] = BroadcastIO[Status](
      uid,
      delta => status = status.merge(delta),
      overlay = Some(DirectConnectionOverlay(selfInfo)),
    )

    def publishStatus(round: Long): Unit = {
      given LocalUid = uid
      val delta      = OverlayStatusProtocol.statusDelta(status, io, timestamp = round)
      status = status.merge(delta)
      io.applyDelta(delta)
    }
  }

  private def connect(queue: LocalMessageQueue, left: Node, right: Node, label: String): Unit = {
    val link = QueuedLocalConnection(queue)
    left.io.addBinaryConnection(link.server)
    right.io.addBinaryConnection(link.client(label))
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
    nodes.foreach(_.io.repairTick())
    drain(queue)

    (1L to 6L).foreach { round =>
      nodes.foreach(_.publishStatus(round))
      drain(queue)
      nodes.foreach(_.io.repairTick())
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
}
