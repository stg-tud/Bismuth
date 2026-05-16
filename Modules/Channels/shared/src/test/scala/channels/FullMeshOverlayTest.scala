package channels

import channels.connection.{Connection, ConnectionDescriptor, ConnectionInfo, MessageBuffer, PeerConnectInfo}
import channels.overlay.FullMeshOverlay
import channels.overlay.OverlayController.{OverlayAction, OverlayMessage}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid

class FullMeshOverlayTest extends FunSuite {

  final private case class TestConnection(name: String) extends Connection {
    override def info: ConnectionInfo                                    = ConnectionInfo("name" -> name)
    override def send(message: MessageBuffer): Async[Any, Unit] = Async(())
    override def close(): Unit                                           = ()
    override def toString: String                                        = s"TestConnection($name)"
  }

  private val self = PeerConnectInfo(Uid.predefined("self"), Set(ConnectionDescriptor.QueuedLocal("self")))
  private val peer = PeerConnectInfo(Uid.predefined("peer"), Set(ConnectionDescriptor.QueuedLocal("peer")))

  test("replacing an active peer connection disconnects the old connection") {
    val oldConn = TestConnection("old")
    val newConn = TestConnection("new")

    val (afterFirst0, firstActions) =
      FullMeshOverlay(self).receiveActions(OverlayMessage.Neighbor(peer, highPriority = true), oldConn)
    val afterFirst = afterFirst0.asInstanceOf[FullMeshOverlay]

    assertEquals(afterFirst.active.get(peer.uid), Some(oldConn))
    assertEquals(firstActions, List(OverlayAction.ActiveConnectionAdded(peer.uid)))

    val (afterSecond0, secondActions) =
      afterFirst.receiveActions(OverlayMessage.Neighbor(peer, highPriority = true), newConn)
    val afterSecond = afterSecond0.asInstanceOf[FullMeshOverlay]

    assertEquals(afterSecond.active.get(peer.uid), Some(newConn))
    assertEquals(secondActions, List(OverlayAction.Disconnect(oldConn)))
  }

  test("bootstrap join is answered with known peers and shuffle reply triggers discoverPassive connects") {
    val contact = PeerConnectInfo(Uid.predefined("contact"), Set(ConnectionDescriptor.QueuedLocal("contact")))
    val other   = PeerConnectInfo(Uid.predefined("other"), Set(ConnectionDescriptor.QueuedLocal("other")))
    val conn    = TestConnection("contact")

    val seeded                 = FullMeshOverlay(self, known = Map(other.uid -> other))
    val (joined0, joinActions) = seeded.receiveActions(OverlayMessage.Join(contact), conn)
    val joined                 = joined0.asInstanceOf[FullMeshOverlay]

    assertEquals(joined.active.get(contact.uid), Some(conn))
    assert(joinActions.contains(OverlayAction.ActiveConnectionAdded(contact.uid)))
    assert(joinActions.contains(OverlayAction.Send(
      conn,
      OverlayMessage.ShuffleReply(self.uid, Set(self, other, contact))
    )))

    val fresh                     = FullMeshOverlay(self, active = Map(contact.uid -> conn))
    val (afterInfo0, infoActions) =
      fresh.receiveActions(OverlayMessage.ShuffleReply(contact.uid, Set(contact, other)), conn)
    val afterInfo = afterInfo0.asInstanceOf[FullMeshOverlay]

    assertEquals(
      infoActions,
      List(OverlayAction.SendJoin(
        other.channelConnectors,
        other.uid,
        OverlayMessage.Neighbor(self, highPriority = true)
      ))
    )
  }

  test("neighbor info is remembered and removed on disconnect") {
    val conn                      = TestConnection("peer")
    val (next0, _)                = FullMeshOverlay(self).receiveActions(OverlayMessage.Neighbor(peer, true), conn)
    val next                      = next0.asInstanceOf[FullMeshOverlay]
    val (removed0, removeActions) = next.removeConnection(conn)
    val removed                   = removed0.asInstanceOf[FullMeshOverlay]

    assertEquals(next.known.get(peer.uid), Some(peer))
    assertEquals(removed.known.get(peer.uid), None)
    assertEquals(removeActions, List(OverlayAction.ActiveConnectionRemoved(peer.uid)))
  }

  test("discovering peers still uses direct neighbor connects and bootstrapping only contacts the bootstrap node") {
    val bootstrap = ConnectionDescriptor.QueuedLocal("bootstrap")
    val other     = PeerConnectInfo(Uid.predefined("other"), Set(ConnectionDescriptor.QueuedLocal("other")))

    val (discovered0, discoverActions) = FullMeshOverlay(self).discoverPassive(Set(other))
    val discovered                     = discovered0.asInstanceOf[FullMeshOverlay]

    assertEquals(
      discoverActions,
      List(OverlayAction.SendJoin(
        other.channelConnectors,
        other.uid,
        OverlayMessage.Neighbor(self, highPriority = true)
      ))
    )
    assertEquals(discovered.known.get(other.uid), None)

    val (_, bootstrapActions) = discovered.bootstrapVia(bootstrap)
    assertEquals(
      bootstrapActions,
      List(OverlayAction.SendJoin(Set(bootstrap), self.uid, OverlayMessage.Join(self)))
    )
  }
}
