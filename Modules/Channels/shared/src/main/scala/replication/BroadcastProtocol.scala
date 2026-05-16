package replication

import rdts.base.Uid
import replication.PlumtreeBroadcast.{Peer, Result}
import replication.PlumtreeMessage.*

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

  def addPeer(peer: Peer): Result[State]
  def removePeer(peer: Peer): Result[State]

  def broadcast(delta: State): Result[State]

  def tick(): Result[State]

  def handleMessage(from: Peer, message: PlumtreeMessage[State]): Result[State]
}
