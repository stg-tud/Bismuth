package channels.routing

import channels.JsoniterCodecs.given
import channels.routing.PlumtreeBroadcast.Event.Send
import channels.routing.PlumtreeBroadcast.PeerRole
import channels.routing.PlumtreeMessage.*
import channels.routing.Routing.{Effect, Step}
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

/** What plumtree needs to know about its `Payload`s, `Knowledge` summaries, and local `Store`.
  * Like [[Routing]], the related types are members, fixed by a stable instance.
  *
  * Knowledge is used in two roles: the exact content of a payload ([[content]]), and a summary of everything a replica
  * has ([[summary]], sent in `Graft`). Both only need to be understood by the store: [[missing]] tells which part
  * of some knowledge the store lacks, and [[replay]] which payloads a replica with some knowledge lacks.
  *
  * For [[rdts.time.Dots]] both roles coincide. For hash DAGs, content is the hashes of the events in a payload,
  * while the summary is the heads, standing for their causal past.
  */
trait PlumtreeKnowledge {
  type Store
  type Knowledge
  type Payload

  final type Msg = PlumtreeMessage[Knowledge, Payload]

  extension (knowledge: Knowledge) {
    def merge(other: Knowledge): Knowledge
    def isEmpty: Boolean
  }

  extension (payload: Payload) {

    /** Exact knowledge carried by a payload, used to detect duplicates and announced in `IHave`. */
    def content: Knowledge

    /** Knowledge a sender of the payload has, e.g., its content and everything it depends on. */
    def implied: Knowledge = payload.content
  }

  extension (store: Store) {

    /** Summary of everything in the store, sent in `Graft`. */
    def summary: Knowledge

    /** The part of `knowledge` the store does not have. */
    def missing(knowledge: Knowledge): Knowledge

    /** Add a payload whose content was not fully known. Returns the payloads to deliver because of it. */
    def remember(payload: Payload): (Store, List[Payload])

    /** Payloads in the store that a replica knowing `knows` may lack, in an order it can receive them. */
    def replay(knows: Knowledge): List[Payload]

    /** Knowledge announced by peers, kept in the store as it is typed by [[Knowledge]]. */
    def remote: Map[Peer, Knowledge]
    def withRemote(remote: Map[Peer, Knowledge]): Store

    /** Remote knowledge to keep for repair; may drop what the store has, which is never needed again. */
    def compact(knowledge: Knowledge): Knowledge = knowledge

    final def has(knowledge: Knowledge): Boolean = store.missing(knowledge).isEmpty
  }
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

  def apply[State](
      localContext: Dots = Dots.empty,
      remoteContext: Map[Peer, Dots] = Map.empty,
      deltaStorage: DeltaStorage[State] = NoHistory[State](),
      peerRoles: Map[Peer, PeerRole] = Map.empty,
      repairScheduled: Boolean = false,
  ): PlumtreeBroadcast[State] =
    PlumtreeCore(DotsStore(localContext, deltaStorage, remoteContext), peerRoles, repairScheduled)

  final class Protocol[S] extends Plumtree(DotsKnowledge[S]())

  given protocol[State]: Protocol[State] = Protocol[State]()

  /** Local operation: assign the next dot of `self` to `data` and disseminate it. */
  def broadcastLocal[State](self: Uid, data: State)(state: PlumtreeBroadcast[State]): (res: Result[State], dot: Dot) = {
    val dot = state.localContext.nextDot(self)
    (protocol[State].broadcast(state, Payload(Dots.single(dot), data)), dot)
  }

  /** Application API of a [[MessagingIO]] running plumtree. */
  extension [State](io: MessagingIO[Protocol[State]]) {

    /** Disseminate a local delta and return the dot assigned to it (`None` if `io` is closed).
      * Receivers get the same dot as part of the [[channels.routing.PlumtreeMessage.Payload]], which allows applications to track causality.
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
type PlumtreeBroadcast[State] = PlumtreeCore[DotsStore[State]]

final case class PlumtreeCore[S](
    store: S,
    peerRoles: Map[Peer, PeerRole] = Map.empty,
    repairScheduled: Boolean = false,
) {
  def eagerPeers: Set[Peer] = peerRoles.collect { case (peer, PeerRole.Eager) => peer }.toSet

  /** roles are only added/removed by the add/remove peer methods, so here we only change it if a role exists */
  def withRole(peer: Peer, role: PeerRole): PlumtreeCore[S] =
    copy(peerRoles = peerRoles.updated(peer, role))
}

/** Transitions of plumtree, the state is a [[PlumtreeCore]], generic over the [[PlumtreeKnowledge]]. */
class Plumtree[KN <: PlumtreeKnowledge](val knowledge: KN) extends Routing {
  import PlumtreeBroadcast.*
  import knowledge.*

  type State = PlumtreeCore[knowledge.Store]
  type Msg   = knowledge.Msg
  type Out   = knowledge.Payload

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
    Step(next, List(Send(peer :: Nil, Graft(state.store.summary))))
  }

  /** Algorithm 3 `NeighborDown(node)`. */
  def removePeer(state: State, peer: Peer): Result = Step(
    state.copy(
      peerRoles = state.peerRoles.removed(peer),
      store = state.store.withRemote(state.store.remote.removed(peer))
    ),
    Nil
  )

  /** Local broadcast, corresponding to Algorithm 1 `Broadcast`/`EagerPush`/`LazyPush`. */
  def broadcast(state: State, payload: Out): Result = {
    val (store, delivered) = state.store.remember(payload)
    val next               = state.copy(store = store)
    // local payloads are not delivered, but payloads they released are
    val sent = disseminate(next, payload, Set.empty)
    sent.copy(events = delivered.filterNot(_ == payload).map(Event.Deliver(_)) ++ sent.events)
  }

  /** Peers that announced knowledge we do not have yet. */
  private def peersWithMissing(state: State): Iterable[Peer] =
    state.store.remote.collect { case (peer, context) if !state.store.has(context) => peer }

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
                List(Send(List(missing), Graft(state.store.summary)))
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
        val relevant = next.store.replay(knows)
        val replayed = relevant.map(payload => Send(from :: Nil, Push(payload)))
        scheduleRepairIfNeeded(next, replayed)

      case Push(payload) =>
        if state.store.has(payload.content) then
            // Duplicate eager path: demote this edge and ask the sender to prune too.
            Step(state.withRole(from, PeerRole.Lazy), List(Send(from :: Nil, Prune)))
        else
            // First delivery: remember, keep sender eager, deliver, and forward.
            val (store, delivered) = state.store.remember(payload)
            val next               = withRemoteKnowledge(
              compactRemote(state.copy(store = store)),
              from,
              payload.implied
            ).withRole(from, PeerRole.Eager)
            // payloads the store rejected are not forwarded
            // the sender may also have announced dependencies we lack
            if !store.has(payload.content) then scheduleRepairIfNeeded(next, Nil)
            else
                val forwarded = disseminate(next, payload, except = Set(from))
                scheduleRepairIfNeeded(forwarded.state, delivered.map(Event.Deliver(_)) ++ forwarded.events)

  private def withRemoteKnowledge(state: State, from: Peer, knows: Knowledge): State =
    state.copy(store = state.store.withRemote(state.store.remote.updatedWith(from) {
      case None     => Some(state.store.compact(knows))
      case Some(ex) => Some(state.store.compact(ex `merge` knows))
    }))

  private def compactRemote(state: State): State =
    state.copy(store = state.store.withRemote(state.store.remote.view.mapValues(state.store.compact).toMap))

  /** Send payload on eager edges and `IHave` on lazy edges. */
  private def disseminate(state: State, payload: Out, except: Set[Peer]): Result = {
    val recipients = state.peerRoles.keySet -- except
    val eager      = recipients.filter(peer => state.peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers  = recipients.filter(peer => state.peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Send(eager, Push(payload): Msg))
    val lazySend  =
      Option.when(lazyPeers.nonEmpty)(Send(lazyPeers, IHave(payload.content): Msg))

    Step(state, List(eagerSend, lazySend).flatten)
  }
}

/** Dots-based store: the local context with the replay history, and the knowledge of peers. */
final case class DotsStore[State](
    localContext: Dots,
    deltaStorage: DeltaStorage[State],
    remoteContext: Map[Peer, Dots] = Map.empty,
)

object DotsStore {

  /** Accessors of [[PlumtreeBroadcast]], here to be in its implicit scope. */
  extension [State](state: PlumtreeBroadcast[State]) {
    def localContext: Dots                 = state.store.localContext
    def deltaStorage: DeltaStorage[State]  = state.store.deltaStorage
    def remoteContext: Map[Peer, Dots]     = state.store.remoteContext
    def allPayloads: List[Payload[State]]  = state.store.deltaStorage.getHistory
  }
}

final class DotsKnowledge[State] extends PlumtreeKnowledge {
  type Store     = DotsStore[State]
  type Knowledge = Dots
  type Payload   = PlumtreeMessage.Payload[State]

  extension (knowledge: Dots) {
    def merge(other: Dots): Dots = rdts.base.Lattice.merge(knowledge, other)
    def isEmpty: Boolean         = knowledge.isEmpty
  }

  extension (payload: Payload) def content: Dots = payload.dots

  extension (store: Store) {
    def summary: Dots                      = store.localContext
    def missing(knowledge: Dots): Dots     = knowledge.subtract(store.localContext)
    def remember(payload: Payload): (Store, List[Payload]) =
      (
        store.copy(localContext = store.localContext.merge(payload.dots), deltaStorage = store.deltaStorage.remember(payload)),
        List(payload)
      )
    def replay(knows: Dots): List[Payload] = store.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
    def remote: Map[Peer, Dots]                    = store.remoteContext
    def withRemote(remote: Map[Peer, Dots]): Store = store.copy(remoteContext = remote)
  }
}
