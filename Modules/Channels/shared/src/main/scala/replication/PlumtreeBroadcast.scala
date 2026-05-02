package replication

import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event.Disseminate
import replication.ProtocolMessage.*

object PlumtreeBroadcast {

  /** Overlay neighbor used by the pure Plumtree state machine. */
  final case class Peer(uid: Uid)

  /** Paper: `Eager` ↔ `eagerPushPeers`, `Lazy` ↔ `lazyPushPeers`. */
  enum PeerRole {
    case Eager, Lazy
  }

  /** Paper: `Deliver` ↔ `Deliver(m)`, `Disseminate` ↔ `Send(...)`. */
  enum Event[+State] {
    case Deliver(payload: Payload[State])
    case Disseminate(peers: List[Peer], payload: ProtocolMessage[State])
  }

  final case class Result[State](
      state: PlumtreeBroadcast[State],
      events: List[Event[State]],
  )
}

/** Immutable Plumtree core.
  *
  * Paper mapping:
  * - `peerRoles` ↔ eager/lazy peer sets
  * - `localContext` ↔ delivered/known message summary
  * - `deltaStorage` ↔ replay history used to answer `Graft`
  * - `addPeer`/`removePeer` ↔ `NeighborUp`/`NeighborDown`
  *
  * Main adaptation: instead of per-message timers and `(mID, round)`, this version uses dot contexts.
  * `IHave` and `Graft` summarize knowledge with `Dots`, and `Graft` replays all history newer than `knows`.
  */
final case class PlumtreeBroadcast[State](
    self: Uid,
    localContext: Dots = Dots.empty,
    deltaStorage: DeltaStorage[State] = NoHistory(),
    peerRoles: Map[PlumtreeBroadcast.Peer, PlumtreeBroadcast.PeerRole] = Map.empty,
) {
  import PlumtreeBroadcast.*

  def eagerPeers: Set[Uid] = peerRoles.collect { case (Peer(uid), PeerRole.Eager) => uid }.toSet

  /** Algorithm 3 `NeighborUp(node)`. Adds the peer as eager and requests missing history. */
  def addPeer(peer: Peer): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(peerRoles = peerRoles.updated(peer, PeerRole.Eager))
    Result(next, List(Disseminate(peer :: Nil, Graft(self, localContext))))
  }

  /** Algorithm 3 `NeighborDown(node)`. */
  def removePeer(peer: Peer): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.removed(peer))

  /** Local broadcast, corresponding to Algorithm 1 `Broadcast`/`EagerPush`/`LazyPush`. */
  def broadcast(payload: Payload[State], except: Set[Peer] = Set.empty): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(
      localContext = localContext.merge(payload.dots),
      deltaStorage = deltaStorage.remember(payload),
    )
    next.disseminate(payload, except)
  }

  /** Incoming message handler.
    *
    * Paper mapping:
    * - `Payload`: Algorithm 1 receive/duplicate handling
    * - `Prune`: Algorithm 1 prune handling
    * - `IHave`: Algorithm 2 missing-message detection, without explicit timers
    * - `Graft`: Algorithm 2 repair and replay
    */
  def handleMessage(from: Peer, message: ProtocolMessage[State]): Result[State] =
    message match
      case Ping(time) =>
        Result(this, List(Disseminate(from :: Nil, Pong(time))))

      case Pong(_) =>
        Result(this, Nil)

      case Prune(_) =>
        // Duplicate eager delivery: demote this edge to lazy.
        Result(withRole(from, PeerRole.Lazy), Nil)

      case IHave(_, knows) =>
        // If the sender advertises dots beyond localContext, request repair.
        if !(knows <= localContext) then Result(this, List(Disseminate(from :: Nil, Graft(self, localContext))))
        else Result(this, Nil)

      case Graft(_, knows) =>
        // Repair: promote sender back to eager and replay what it is missing.
        val next = withRole(from, PeerRole.Eager)
        val relevant = next.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
        if relevant.isEmpty && !(next.localContext <= knows) then
          println(s"plumtree graft unsatisfied from=$from local=${next.localContext} knows=$knows storage=${next.deltaStorage.getClass.getSimpleName}")
        Result(next, relevant.map(payload => Disseminate(from :: Nil, payload)))

      case payload @ Payload(context, _, _) =>
        if context <= localContext then
          // Duplicate eager path: demote this edge and ask the sender to prune too.
          Result(withRole(from, PeerRole.Lazy), List(Disseminate(from :: Nil, Prune(self))))
        else
          // First delivery: remember, keep sender eager, deliver, and forward.
          val next: PlumtreeBroadcast[State] = copy(
            localContext = localContext.merge(context),
            deltaStorage = deltaStorage.remember(payload),
          ).withRole(from, PeerRole.Eager)
          val forwarded = next.disseminate(payload, except = Set(from))
          forwarded.copy(events = Event.Deliver(payload) :: forwarded.events)

  private def withRole(peer: Peer, role: PeerRole): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.updated(peer, role))

  /** Send payload on eager edges and `IHave` on lazy edges. */
  private def disseminate(payload: Payload[State], except: Set[Peer]): Result[State] = {
    val recipients = peerRoles.keySet -- except
    val eager = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Disseminate(eager, payload: ProtocolMessage[State]))
    val lazySend = Option.when(lazyPeers.nonEmpty)(Disseminate(lazyPeers, IHave(self, localContext): ProtocolMessage[State]))

    Result(this, List(eagerSend, lazySend).flatten)
  }
}
