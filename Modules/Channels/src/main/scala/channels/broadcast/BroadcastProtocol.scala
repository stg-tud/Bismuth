package channels.broadcast

import channels.broadcast.PlumtreeBroadcast.Result
import channels.connection.Peer
import channels.broadcast.PlumtreeMessage.Payload
import rdts.time.Dots

/** Minimal interface for a broadcast protocol state machine.
  *
  * Implementations manage the eager/lazy peer roles,
  * dot-based causal context, and message dissemination.
  *
  * Currently all implementations use [[PlumtreeMessage]] as
  * the wire format (Graft, IHave, Payload, Prune).
  */
trait BroadcastProtocol[State] {

  def allPayloads: List[Payload[State]]

  /** Dots of all payloads known to this protocol, used to assign dots to new local payloads. */
  def localContext: Dots

  def addPeer(peer: Peer): Result[State]
  def removePeer(peer: Peer): Result[State]

  /** Disseminate a local payload. Its dots are assigned by the caller and must be new. */
  def broadcast(payload: Payload[State]): Result[State]

  /** A timer requested via [[PlumtreeBroadcast.Event.StartTimer]] expired. */
  def timerFired(timer: BroadcastTimer): Result[State]

  /** `from` has been checked by the driver to be a sender the receiving connection may carry. */
  def handleMessage(from: Peer, message: PlumtreeMessage[State]): Result[State]
}

/** Timers carry only their purpose; the driver decides what delay each purpose maps to. */
enum BroadcastTimer {

  /** Check whether peers announced knowledge we still lack, and graft it. */
  case Repair
}
