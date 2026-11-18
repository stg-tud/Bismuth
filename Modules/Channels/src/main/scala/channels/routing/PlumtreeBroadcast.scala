package channels.broadcast

import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast.Event.Send
import channels.broadcast.PlumtreeBroadcast.PeerRole
import channels.broadcast.PlumtreeMessage.*
import channels.broadcast.Routing.{Effect, Step}
import channels.connection.Peer
import channels.{DeltaStorage, MessagingIO, NoHistory}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import rdts.time.{Dot, Dots}

import scala.util.Random

enum PlumtreeMessage[+K, +P] {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case Graft(knows: K)

  case Push(payload: P)

  /** Lazy advertisement for Plumtree-style dissemination.
    * `knows` summarizes what the sender already has.
    */
  case IHave(knows: K)

  /** Request sender to move this edge to lazy mode for eager push. */
  case Prune
}
object PlumtreeMessage {

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[+T](dots: Dots, data: T)
}

object PlumtreeBroadcast {

  /** Paper: `Eager` ↔ `eagerPushPeers`, `Lazy` ↔ `lazyPushPeers`. */
  enum PeerRole { case Eager, Lazy }

  /** Paper: `Deliver` ↔ `Deliver(m)`, `Send` ↔ `Send(...)`.
    * Deliver is to the local system.
    */
  type Message[+State] = PlumtreeMessage[Dots, Payload[State]]

  type Event[+State] = Effect[Message[State], Payload[State]]
  val Event: Effect.type = Effect

  type Result[State] = Step[PlumtreeBroadcast[State], Message[State], Payload[State]]
  val Result: Step.type = Step

  /** Wire format. Not a given, as the derivation would find itself. */
  def messageCodec[State: JsonValueCodec]: JsonValueCodec[Message[State]] = JsonCodecMaker.make

  final class Protocol[S] extends Plumtree[S]

  given protocol[State]: Protocol[State] = Protocol[State]()

  /** Local operation: assign the next dot of `self` to `data` and disseminate it. */
  def broadcastLocal[State](self: Uid, data: State)(state: PlumtreeBroadcast[State]): (res: Result[State], dot: Dot) = {
    val dot = state.localContext.nextDot(self)
    (protocol[State].broadcast(state, Payload(Dots.single(dot), data)), dot)
  }

  /** Application API of a [[MessagingIO]] running plumtree. */
  extension [State](io: MessagingIO[Protocol[State]]) {

    /** Disseminate a local delta and return the dot assigned to it (`None` if `io` is closed).
      * Receivers get the same dot as part of the [[channels.broadcast.PlumtreeMessage.Payload]], which allows applications to track causality.
      */
    def broadcast(delta: State): Unit = io.update(s => broadcastLocal(io.selfPeer.uid, delta)(s).res)

    def allPayloads: List[Payload[State]] = io.state.allPayloads
  }
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
    localContext: Dots = Dots.empty,
    remoteContext: Map[Peer, Dots] = Map.empty,
    deltaStorage: DeltaStorage[State] = NoHistory(),
    peerRoles: Map[Peer, PeerRole] = Map.empty,
    repairScheduled: Boolean = false,
) {
  def eagerPeers: Set[Peer] = peerRoles.collect { case (peer, PeerRole.Eager) => peer }.toSet

  def allPayloads: List[Payload[State]] = deltaStorage.getHistory

  /** roles are only added/removed by the add/remove peer methods, so here we only change it if a role exists */
  def withRole(peer: Peer, role: PeerRole): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.updated(peer, role))
}

/** Transitions of plumtree, the state is [[PlumtreeBroadcast]]. */
class Plumtree[S] extends Routing {
  import PlumtreeBroadcast.*

  type State = PlumtreeBroadcast[S]
  type Msg   = Message[S]
  type Out   = Payload[S]

  type Result = Step[State, Msg, Out]

  /** Algorithm 3 `NeighborUp(node)`. Requests missing history with an initial graft.
    *
    * The paper adds the peer as eager.
    * We instead add the peer as lazy and let the graft exchange promote the edge: receiving the peer's
    * graft promotes it to eager (see the `Graft` case in [[handleMessage]]). This guarantees that a
    * payload is never both eagerly pushed and replayed by the graft handler to the same peer – such a
    * duplicate would be misread as a redundant mesh edge and cause a spurious prune of the only edge.
    *
    * Note that the peer may already have a role when the overlay reports NeighborUp: the peer's graft
    * can arrive before the overlay's connection notification (e.g. with reentrant transports), in which
    * case the graft handler already promoted the edge to eager. Never downgrade such an edge.
    */
  def addPeer(state: State, peer: Peer): Result = {
    val next = if state.peerRoles.contains(peer) then state else state.withRole(peer, PeerRole.Lazy)
    Step(next, List(Send(peer :: Nil, Graft(state.localContext))))
  }

  /** Algorithm 3 `NeighborDown(node)`. */
  def removePeer(state: State, peer: Peer): Result = Step(
    state.copy(
      peerRoles = state.peerRoles.removed(peer),
      remoteContext = state.remoteContext.removed(peer)
    ),
    Nil
  )

  /** Local broadcast, corresponding to Algorithm 1 `Broadcast`/`EagerPush`/`LazyPush`. */
  def broadcast(state: State, payload: Out): Result = {
    val next = state.copy(
      localContext = state.localContext.merge(payload.dots),
      deltaStorage = state.deltaStorage.remember(payload),
    )
    disseminate(next, payload, Set.empty)
  }

  /** Peers that announced knowledge we do not have yet. */
  private def peersWithMissing(state: State): Iterable[Peer] =
    state.remoteContext.collect { case (peer, context) if context.inflates(state.localContext) => peer }

  /** The paper starts a timer per missing message after an `IHave`. We keep a single repair timer running while
    * any peer announced knowledge we lack; if the knowledge is still missing when it fires, we graft it.
    */
  def timerFired(state: State, timer: Routing.Timer): Result = timer match
      case Routing.Timer.Repair =>
        val unscheduled = state.copy(repairScheduled = false)
        // note, this may very well select already eager peers, which is done to backfill in case of missing state
        Random.shuffle(peersWithMissing(unscheduled).toList).headOption match
            case None          => Step(unscheduled, Nil)
            case Some(missing) =>
              scheduleRepairIfNeeded(
                unscheduled.withRole(missing, PeerRole.Eager),
                List(Send(List(missing), Graft(state.localContext)))
              )

  private def scheduleRepairIfNeeded(state: State, events: Seq[Effect[Msg, Out]]): Result =
    if state.repairScheduled || peersWithMissing(state).isEmpty then Step(state, events)
    else Step(state.copy(repairScheduled = true), events :+ Event.StartTimer(Routing.Timer.Repair))

  /** Incoming message handler.
    *
    * Paper mapping:
    * - `Push`: Algorithm 1 receive/duplicate handling
    * - `Prune`: Algorithm 1 prune handling
    * - `IHave`: Algorithm 2 missing-message detection, without explicit timers
    * - `Graft`: Algorithm 2 repair and replay
    */
  def handleMessage(state: State, from: Peer, message: Msg): Result = message match
      case Prune =>
        // Duplicate eager delivery: demote this edge to lazy.
        Step(state.withRole(from, PeerRole.Lazy), Nil)

      case IHave(knows) =>
        // note, in case we dont know `from` we get into a bit inconsistent state, where its not a peer.
        // this means we wont send it any IHave messages ourselves, thats probably fine?
        scheduleRepairIfNeeded(withRemoteKnowledge(state, from, knows), Nil)

      case Graft(knows) =>
        // Repair: promote sender back to eager and replay what it is missing.
        val next     = withRemoteKnowledge(state.withRole(from, PeerRole.Eager), from, knows)
        val relevant = next.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
        if relevant.isEmpty && !(next.localContext <= knows) then
            println(
              s"plumtree graft unsatisfied from=$from local=${next.localContext} knows=$knows storage=${next.deltaStorage.getClass.getSimpleName}"
            )
        val replayed = relevant.map(payload => Send(from :: Nil, Push(payload)))
        scheduleRepairIfNeeded(next, replayed)

      case Push(payload @ Payload(context, _)) =>
        if context <= state.localContext then
            // Duplicate eager path: demote this edge and ask the sender to prune too.
            Step(state.withRole(from, PeerRole.Lazy), List(Send(from :: Nil, Prune)))
        else
            // First delivery: remember, keep sender eager, deliver, and forward.
            val next = withRemoteKnowledge(
              state.copy(
                localContext = state.localContext.merge(context),
                deltaStorage = state.deltaStorage.remember(payload),
              ),
              from,
              context
            ).withRole(from, PeerRole.Eager)
            val forwarded = disseminate(next, payload, except = Set(from))
            forwarded.copy(events = Event.Deliver(payload) +: forwarded.events)

  private def withRemoteKnowledge(state: State, from: Peer, knows: Dots): State =
    state.copy(remoteContext = state.remoteContext.updatedWith(from) {
      case None     => Some(knows)
      case Some(ex) => Some(ex `merge` knows)
    })

  /** Send payload on eager edges and `IHave` on lazy edges. */
  private def disseminate(state: State, payload: Out, except: Set[Peer]): Result = {
    val recipients = state.peerRoles.keySet -- except
    val eager      = recipients.filter(peer => state.peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers  = recipients.filter(peer => state.peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Send(eager, Push(payload): Msg))
    val lazySend  =
      Option.when(lazyPeers.nonEmpty)(Send(lazyPeers, IHave(payload.dots): Msg))

    Step(state, List(eagerSend, lazySend).flatten)
  }
}
