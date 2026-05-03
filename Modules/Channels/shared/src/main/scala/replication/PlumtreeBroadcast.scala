package replication

import rdts.base.Historized.MetaDelta
import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event.Disseminate
import replication.PlumtreeMessage.*

sealed trait PlumtreeMessage[+T]
object PlumtreeMessage {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case class Graft(sender: Uid, knows: Dots) extends PlumtreeMessage[Nothing]

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[+T](dots: Dots, data: T, redundantDots: Dots)
      extends PlumtreeMessage[T] {}
  object Payload                 {
    def apply[T](dots: Dots, data: T): Payload[T] =
      Payload(dots, data, Dots.empty)

    extension [T](payloads: Iterable[Payload[T]]) {
      inline def getAllDots: Dots =
        payloads.foldLeft(Dots.empty)((dots, payload) => dots.union(payload.dots.union(payload.redundantDots)))

      inline def mapDeltas[A](f: T => A): Iterable[Payload[A]] =
        payloads.map(bufferedPayload => bufferedPayload.copy(data = f(bufferedPayload.data)))

      inline def toMetaDeltas: Iterable[MetaDelta[T]] =
        payloads.map(payload => MetaDelta(payload.dots, payload.data, payload.redundantDots))
    }
  }

  /** Lazy advertisement for Plumtree-style dissemination.
    * `knows` summarizes what the sender already has.
    */
  case class IHave(sender: Uid, knows: Dots) extends PlumtreeMessage[Nothing]

  /** Request sender to move this edge to lazy mode for eager push. */
  case class Prune(sender: Uid) extends PlumtreeMessage[Nothing]
}

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
    case Disseminate(peers: List[Peer], payload: PlumtreeMessage[State])
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
    missingByAge: Vector[Map[PlumtreeBroadcast.Peer, Dots]] = Vector(Map.empty, Map.empty),
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
    copy(
      peerRoles = peerRoles.removed(peer),
      missingByAge = missingByAge.map(_.removed(peer))
    )

  /** Local broadcast, corresponding to Algorithm 1 `Broadcast`/`EagerPush`/`LazyPush`. */
  def broadcast(payload: Payload[State], except: Set[Peer] = Set.empty): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(
      localContext = localContext.merge(payload.dots),
      deltaStorage = deltaStorage.remember(payload),
    ).clearSatisfiedMissing
    next.disseminate(payload, except)
  }

  def repairTick(): Result[State] = {
    val older       = missingByAge.lift(1).getOrElse(Map.empty).filter { case (_, knows) => !(knows <= localContext) }
    val nextBuckets = Vector(
      Map.empty[Peer, Dots],
      missingByAge.headOption.getOrElse(Map.empty).filter { case (_, knows) => !(knows <= localContext) }
    )
    val next   = copy(missingByAge = nextBuckets)
    val grafts = older.keys.toList.map(peer => Disseminate(peer :: Nil, Graft(self, localContext)))
    Result(next, grafts)
  }

  /** Incoming message handler.
    *
    * Paper mapping:
    * - `Payload`: Algorithm 1 receive/duplicate handling
    * - `Prune`: Algorithm 1 prune handling
    * - `IHave`: Algorithm 2 missing-message detection, without explicit timers
    * - `Graft`: Algorithm 2 repair and replay
    */
  def handleMessage(from: Peer, message: PlumtreeMessage[State]): Result[State] =
    message match
        case Prune(_) =>
          // Duplicate eager delivery: demote this edge to lazy.
          Result(withRole(from, PeerRole.Lazy), Nil)

        case IHave(_, knows) =>
          // Record potentially missing knowledge and let repairTick decide when to graft.
          if !(knows <= localContext) then
              val youngest        = missingByAge.headOption.getOrElse(Map.empty)
              val updatedYoungest = youngest.updatedWith(from) {
                case Some(existing) => Some(existing.union(knows))
                case None           => Some(knows)
              }
              Result(copy(missingByAge = missingByAge.updated(0, updatedYoungest)), Nil)
          else Result(this, Nil)

        case Graft(_, knows) =>
          // Repair: promote sender back to eager and replay what it is missing.
          val next     = withRole(from, PeerRole.Eager)
          val relevant = next.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
          if relevant.isEmpty && !(next.localContext <= knows) then
              println(
                s"plumtree graft unsatisfied from=$from local=${next.localContext} knows=$knows storage=${next.deltaStorage.getClass.getSimpleName}"
              )
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
              ).withRole(from, PeerRole.Eager).clearSatisfiedMissing
              val forwarded = next.disseminate(payload, except = Set(from))
              forwarded.copy(events = Event.Deliver(payload) :: forwarded.events)

  private def withRole(peer: Peer, role: PeerRole): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.updated(peer, role))

  private def clearSatisfiedMissing: PlumtreeBroadcast[State] =
    copy(missingByAge = missingByAge.map(_.filter { case (_, knows) => !(knows <= localContext) }))

  /** Send payload on eager edges and `IHave` on lazy edges. */
  private def disseminate(payload: Payload[State], except: Set[Peer]): Result[State] = {
    val recipients = peerRoles.keySet -- except
    val eager      = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers  = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Disseminate(eager, payload: PlumtreeMessage[State]))
    val lazySend  =
      Option.when(lazyPeers.nonEmpty)(Disseminate(lazyPeers, IHave(self, localContext): PlumtreeMessage[State]))

    Result(this, List(eagerSend, lazySend).flatten)
  }
}
