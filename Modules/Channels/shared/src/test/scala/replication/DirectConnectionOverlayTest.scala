package replication

import channels.{Connection, ConnectionInfo, PeerConnectInfo}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}
import replication.overlay.DirectConnectionOverlay

class DirectConnectionOverlayTest extends FunSuite {

  final private case class TestConnection(name: String) extends Connection {
    override def info: ConnectionInfo                                    = ConnectionInfo("name" -> name)
    override def send(message: channels.MessageBuffer): Async[Any, Unit] = Async(())
    override def close(): Unit                                           = ()
    override def toString: String                                        = s"TestConnection($name)"
  }

  private val self = PeerConnectInfo(Uid.predefined("self"))
  private val peer = PeerConnectInfo(Uid.predefined("peer"))

  test("replacing an active peer connection disconnects the old connection") {
    val oldConn = TestConnection("old")
    val newConn = TestConnection("new")

    val (afterFirst0, firstActions) =
      DirectConnectionOverlay(self).receiveActions(OverlayMessage.Neighbor(peer, highPriority = true), oldConn)
    val afterFirst = afterFirst0.asInstanceOf[DirectConnectionOverlay]

    assertEquals(afterFirst.active.get(peer.uid), Some(oldConn))
    assertEquals(firstActions, List(OverlayAction.ActiveConnectionAdded(peer.uid)))

    val (afterSecond0, secondActions) =
      afterFirst.receiveActions(OverlayMessage.Neighbor(peer, highPriority = true), newConn)
    val afterSecond = afterSecond0.asInstanceOf[DirectConnectionOverlay]

    assertEquals(afterSecond.active.get(peer.uid), Some(newConn))
    assertEquals(secondActions, List(OverlayAction.Disconnect(oldConn)))
  }
}
