package channels

import channels.connection.{Connection, ConnectionDescriptor, ConnectionInfo, MessageBuffer, PeerConnectInfo}
import channels.overlay.FullMeshOverlay
import channels.overlay.OverlayController.{OverlayAction, OverlayMessage}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid

/** Pins down that a locally activated connection does not optimistically emit ActiveConnectionAdded.
  *
  * The overlay must not report the peer as an active connection just because we established a local
  * transport; it needs to be confirmed remotely first (via a received Neighbor/Join handshake).
  */
class FullMeshActivationTest extends FunSuite {

  final private case class TestConnection(name: String) extends Connection {
    override def info: ConnectionInfo                           = ConnectionInfo("name" -> name)
    override def send(message: MessageBuffer): Async[Any, Unit] = Async(())
    override def close(): Unit                                  = ()
    override def toString: String                               = s"TestConnection($name)"
  }

  private val self = PeerConnectInfo(Uid.predefined("self"), Set(ConnectionDescriptor.QueuedLocal("self")))
  private val peer = PeerConnectInfo(Uid.predefined("peer"), Set(ConnectionDescriptor.QueuedLocal("peer")))

  test("activating a raw local connection does not emit ActiveConnectionAdded") {
    val conn                     = TestConnection("peer")
    val (_, activationActions)   = FullMeshOverlay(self).activateConnection(conn, None)
    assert(!activationActions.contains(OverlayAction.ActiveConnectionAdded(peer.uid)))
  }

  test("activating a local connection with connect info does not emit ActiveConnectionAdded") {
    val conn                   = TestConnection("peer")
    val descriptor             = peer.channelConnectors.head
    val (_, activationActions) = FullMeshOverlay(self).activateConnection(conn, Some(descriptor))
    assert(!activationActions.contains(OverlayAction.ActiveConnectionAdded(peer.uid)))
  }

  test("receiveActions trusts the peer identity in the message, attaching whatever conn carried it") {
    // A connection that is NOT the one leading to `peer` (no transport back to the claimed peer).
    val wrongConnection         = TestConnection("some-other-connection")
    val stranger                = PeerConnectInfo(Uid.predefined("stranger"), Set.empty)

    // The message simply *claims* to be a Neighbor from `stranger`. The overlay adopts the peer
    // identity from the message and attaches the conn the message was carried on, without any
    // verification that this conn actually connects back to `stranger`.
    val (next0, actions) =
      FullMeshOverlay(self).receiveActions(OverlayMessage.Neighbor(stranger, highPriority = true), wrongConnection)
    val next = next0.asInstanceOf[FullMeshOverlay]

    assert(actions.contains(OverlayAction.ActiveConnectionAdded(stranger.uid)))
    assertEquals(next.active.get(stranger.uid), Some(wrongConnection))
    assertEquals(next.connectionFor(stranger.uid), Some(wrongConnection))
  }
}
