package replication

import rdts.base.{Lattice, Uid}
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event.Send
import replication.PlumtreeBroadcast.{Peer, PeerRole}
import replication.PlumtreeMessage.*

import scala.util.Random

sealed trait PlumtreeMessage[+T]
object PlumtreeMessage {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case class Graft(knows: Dots) extends PlumtreeMessage[Nothing]

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[+T](dots: Dots, data: T) extends PlumtreeMessage[T]

  // exists so we can merge history
  given [T: Lattice] => Lattice[Payload[T]] = Lattice.derived

  /** Lazy advertisement for Plumtree-style dissemination.
    * `knows` summarizes what the sender already has.
    */
  case class IHave(knows: Dots) extends PlumtreeMessage[Nothing]

  /** Request sender to move this edge to lazy mode for eager push. */
  case object Prune extends PlumtreeMessage[Nothing]
}

object PlumtreeBroadcast {

  /** Overlay neighbor used by the pure Plumtree state machine. */
  final case class Peer(uid: Uid)

  /** Paper: `Eager` ↔ `eagerPushPeers`, `Lazy` ↔ `lazyPushPeers`. */
  enum PeerRole { case Eager, Lazy }

  /** Paper: `Deliver` ↔ `Deliver(m)`, `Send` ↔ `Send(...)`.
    * Deliver is to the local system.
    */
  enum Event[+State] {
    case Deliver(payload: Payload[State])
    case Send(peers: List[Peer], message: PlumtreeMessage[State])
  }

  final case class Result[State](
      state: PlumtreeBroadcast[State],
      events: Seq[Event[State]],
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
    remoteContext: Map[Peer, Dots] = Map.empty,
    deltaStorage: DeltaStorage[State] = NoHistory(),
    peerRoles: Map[Peer, PeerRole] = Map.empty,
    remoteContextSnapshot: Map[Peer, Dots] = Map.empty,
) extends BroadcastProtocol[State] {
  import PlumtreeBroadcast.*

  def eagerPeers: Set[Uid] = peerRoles.collect { case (Peer(uid), PeerRole.Eager) => uid }.toSet

  /** Algorithm 3 `NeighborUp(node)`. Adds the peer as eager and requests missing history. */
  def addPeer(peer: Peer): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(peerRoles = peerRoles.updated(peer, PeerRole.Eager))
    Result(next, List(Send(peer :: Nil, Graft(localContext))))
  }

  def allPayloads: List[Payload[State]] = deltaStorage.getHistory

  /** Algorithm 3 `NeighborDown(node)`. */
  def removePeer(peer: Peer): Result[State] = Result(
    copy(
      peerRoles = peerRoles.removed(peer),
      remoteContext = remoteContext.removed(peer)
    ),
    Nil
  )

  /** Local broadcast, corresponding to Algorithm 1 `Broadcast`/`EagerPush`/`LazyPush`. */
  def broadcast(delta: State): Result[State] = {
    val nextDot = localContext.nextDot(self)
    val payload = Payload(Dots.single(nextDot), delta)

    val next: PlumtreeBroadcast[State] = copy(
      localContext = localContext.merge(payload.dots),
      deltaStorage = deltaStorage.remember(payload),
    )
    next.disseminate(payload, Set.empty)
  }

  /** The paper manages some timeouts after IHave messages, we instead check that the local context catches up between any two tick grafts calls. */
  def tick(): Result[State] = {
    // note, this may very well select already active peers, which is done to backfill in case of missing state
    val haveMissing = remoteContextSnapshot.collect:
        case (peer, context) if context.inflates(localContext) => peer
    Random.shuffle(haveMissing).headOption match {
      case None          => Result(copy(remoteContextSnapshot = remoteContext), Nil)
      case Some(missing) =>
        val graft = Send(List(missing), Graft(localContext))
        val next  = copy(remoteContextSnapshot = remoteContext).withRole(missing, PeerRole.Eager)
        Result(next, List(graft))
    }

  }

  /** Incoming message handler.
    *
    * Paper mapping:
    * - `Payload`: Algorithm 1 receive/duplicate handling
    * - `Prune`: Algorithm 1 prune handling
    * - `IHave`: Algorithm 2 missing-message detection, without explicit timers
    * - `Graft`: Algorithm 2 repair and replay
    */
  def handleMessage(from: Peer, message: PlumtreeMessage[State]): Result[State] = message match
      case Prune =>
        // Duplicate eager delivery: demote this edge to lazy.
        Result(withRole(from, PeerRole.Lazy), Nil)

      case IHave(knows) =>
        // note, in case we dont know `from` we get into a bit inconsistent state, where its not a peer.
        // this means we wont send it any IHave messages ourselves, thats probably fine?
        Result(withRemoteKnowledge(from, knows), Nil)

      case Graft(knows) =>
        // Repair: promote sender back to eager and replay what it is missing.
        val next     = withRole(from, PeerRole.Eager).withRemoteKnowledge(from, knows)
        val relevant = next.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
        if relevant.isEmpty && !(next.localContext <= knows) then
            println(
              s"plumtree graft unsatisfied from=$from local=${next.localContext} knows=$knows storage=${next.deltaStorage.getClass.getSimpleName}"
            )
        val replayed = relevant.map(payload => Send(from :: Nil, payload))
        Result(next, replayed)

      case payload @ Payload(context, _) =>
        if context <= localContext then
            // Duplicate eager path: demote this edge and ask the sender to prune too.
            Result(withRole(from, PeerRole.Lazy), List(Send(from :: Nil, Prune)))
        else
            // First delivery: remember, keep sender eager, deliver, and forward.
            val next: PlumtreeBroadcast[State] = copy(
              localContext = localContext.merge(context),
              deltaStorage = deltaStorage.remember(payload),
            ).withRemoteKnowledge(from, context).withRole(from, PeerRole.Eager)
            val forwarded = next.disseminate(payload, except = Set(from))
            forwarded.copy(events = Event.Deliver(payload) +: forwarded.events)

  private def withRemoteKnowledge(from: Peer, knows: Dots) = {
    copy(remoteContext = remoteContext.updatedWith(from) {
      case None     => Some(knows)
      case Some(ex) => Some(ex `merge` knows)
    })
  }

  /** roles are only added/removed by the add/remove peer methods, so here we only change it if a role exists */
  private def withRole(peer: Peer, role: PeerRole): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.updated(peer, role))

  /** Send payload on eager edges and `IHave` on lazy edges. */
  private def disseminate(payload: Payload[State], except: Set[Peer]): Result[State] = {
    val recipients = peerRoles.keySet -- except
    val eager      = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers  = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Send(eager, payload: PlumtreeMessage[State]))
    val lazySend  =
      Option.when(lazyPeers.nonEmpty)(Send(lazyPeers, IHave(payload.dots): PlumtreeMessage[State]))

    Result(this, List(eagerSend, lazySend).flatten)
  }
}
