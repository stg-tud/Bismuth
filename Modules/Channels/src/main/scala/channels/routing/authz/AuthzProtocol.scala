package channels.routing.authz

import channels.MessagingIO
import AuthzProtocol.identityOf
import channels.routing.Routing.{Effect, Step, Timer}
import channels.authz.{AuthzEvent, AuthzReplica}
import channels.routing.{Plumtree, PlumtreeCore, PlumtreeKnowledge, PlumtreeMessage, Routing}
import channels.connection.{Peer, PeerConnectInfo}
import channels.crypto.{Hash, PrivateIdentity, PublicIdentity, Verifier}
import channels.overlay.FullMeshOverlay
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import rdts.base.{Bottom, Decompose, Lattice, ReplicaId, Uid}
import rdts.filters.{Permits, PermissionTree}

import scala.util.Try

/** [[Plumtree]] store of an [[channels.authz.AuthzReplica]], with the event hashes peers announced that the replica lacks. */
final case class AuthzStore[RDT](replica: AuthzReplica[RDT], remoteMissing: Map[Peer, Set[Hash]] = Map.empty)

/** Plumtree over the events of an [[channels.authz.AuthzReplica]]: payloads are encoded events, knowledge are event hashes, and the
  * summary are the heads. Replay uses the cuts of the [[channels.authz.AuthzEventGraph]], see [[channels.authz.AuthzEventGraph.notBefore]].
  */
final class AuthzKnowledge[RDT] extends PlumtreeKnowledge {
  type Store     = AuthzStore[RDT]
  type Knowledge = Set[Hash]
  type Payload   = Array[Byte]

  extension (knowledge: Set[Hash]) {
    def merge(other: Set[Hash]): Set[Hash] = knowledge ++ other
    def isEmpty: Boolean                   = knowledge.isEmpty
  }

  extension (payload: Array[Byte]) {
    def content: Set[Hash] = Set(Hash.compute(payload))

    /** The sender has the event and everything it depends on. */
    override def implied: Set[Hash] =
      payload.content ++ Try(AuthzReplica.dependencies(readFromArray[AuthzEvent](payload))).getOrElse(Set.empty)
  }

  extension (store: Store) {
    def summary: Set[Hash]                                = store.replica.heads
    def missing(knowledge: Set[Hash]): Set[Hash]          = knowledge.filterNot(store.replica.received)
    override def compact(knowledge: Set[Hash]): Set[Hash] = store.missing(knowledge)
    def remember(payload: Array[Byte]): (Store, List[Array[Byte]]) =
      val (replica, added) = store.replica.receiveEncoded(payload)
      (store.copy(replica = replica), added)
    def replay(knows: Set[Hash]): List[Array[Byte]]     = store.replica.notBefore(knows)
    def remote: Map[Peer, Set[Hash]]                    = store.remoteMissing
    def withRemote(remote: Map[Peer, Set[Hash]]): Store = store.copy(remoteMissing = remote)
  }
}

/** Authorization-aware replication: the events of an [[channels.authz.AuthzReplica]] are disseminated by [[Plumtree]], and the delta
  * values follow the events they belong to, filtered for what each receiver may read. Delivers the materialized state
  * whenever it changes. `identity` signs local events.
  *
  * Only authenticated peers ([[channels.connection.Peer.Authenticated]]) take part: their uid is their
  * [[channels.crypto.PublicIdentity]], which is what capabilities are granted to, and what deltas are filtered for.
  */
final class AuthzProtocol[RDT](val identity: PrivateIdentity) extends Routing {
  type State = PlumtreeCore[AuthzStore[RDT]]
  type Msg   = AuthzMessage
  type Out   = RDT

  type Result = Step[State, Msg, Out]

  private val events = Plumtree(AuthzKnowledge[RDT]())

  def addPeer(state: State, peer: Peer): Result = identityOf(peer) match
      case None    => Step(state, Nil)
      case Some(_) =>
        val step = lift(state, events.addPeer(state, peer))
        step.copy(events = step.events ++ requestDeltas(step.state, List(peer)))

  def removePeer(state: State, peer: Peer): Result = lift(state, events.removePeer(state, peer))

  def timerFired(state: State, timer: Timer): Result = lift(state, events.timerFired(state, timer))

  def handleMessage(state: State, from: Peer, message: AuthzMessage): Result = identityOf(from) match
      case None         => Step(state, Nil)
      case Some(_) =>
        message match
            case AuthzMessage.Events(message)       => lift(state, events.handleMessage(state, from, message))
            case AuthzMessage.Deltas(values) =>
              // forward the newly accepted deltas, the events may have been forwarded before the deltas arrived
              val before   = state.store.replica
              val after    = before.receiveDeltas(values)
              val accepted = values.map(_.event).filter(hash => before.delta(hash).isEmpty && after.delta(hash).nonEmpty)
              val forward  = (state.peerRoles.keySet - from).toList.flatMap(deltasFor(after, accepted, _))
              changed(state, withReplica(state, after), forward)
            case AuthzMessage.RequestDeltas(hashes) =>
              Step(state, deltasFor(state.store.replica, hashes.toList, from))

  // ------------------------------------------------------------------------------------------------
  // local operations, throw if not permitted
  // ------------------------------------------------------------------------------------------------

  /** Write `delta` using any active capability that allows it. */
  def write(state: State, delta: RDT): Result = local(state, state.store.replica.write(identity, delta))

  def write(state: State, delta: RDT, capability: Hash): Result =
    local(state, state.store.replica.write(identity, delta, capability))

  def delegate(state: State, usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree)
      : Result = local(state, state.store.replica.delegate(identity, usedCapability, delegatee, read, write))

  def revoke(state: State, revokedCapability: Hash): Result =
    local(state, state.store.replica.revoke(identity, revokedCapability))

  /** Accept a (trusted) event out of band, e.g., the genesis event. */
  def receiveLocally(state: State, event: AuthzEvent): Result =
    local(state, state.store.replica.receiveLocally(event))

  // ------------------------------------------------------------------------------------------------
  // helpers
  // ------------------------------------------------------------------------------------------------

  private def withReplica(state: State, replica: AuthzReplica[RDT]): State =
    state.copy(store = state.store.copy(replica = replica))

  /** Disseminate the events a local operation created. */
  private def local(state: State, local: AuthzReplica.Local[RDT]): Result = {
    val updated = withReplica(state, local.state)
    val step    = local.events.foldLeft(Step(updated, Seq.empty): events.Result) { (acc, event) =>
      val next = events.broadcast(acc.state, event)
      Step(next.state, acc.events ++ next.events)
    }
    lift(state, step)
  }

  /** Wraps the plumtree messages, sends the deltas of pushed events along, and delivers the state if it changed. */
  private def lift(before: State, step: events.Result): Result = {
    val replica = step.state.store.replica
    val pushed  = scala.collection.mutable.LinkedHashMap.empty[Peer, List[Hash]]
    val sends   = step.events.flatMap {
      case Effect.Send(peers, message) =>
        message match
            case PlumtreeMessage.Push(event) =>
              peers.foreach(peer => pushed.updateWith(peer)(hashes => Some(hashes.getOrElse(Nil) :+ Hash.compute(event))))
            case _ =>
        List(Effect.Send(peers, AuthzMessage.Events(message)))
      case Effect.StartTimer(timer) => List(Effect.StartTimer(timer))
      case Effect.Deliver(_)        => Nil
    }
    val deltas = pushed.toList.flatMap((peer, hashes) => deltasFor(replica, hashes, peer))
    // a new capability may make deltas readable that were withheld from us, ask everyone for them
    val capabilitiesBefore = before.store.replica.eventGraph.capabilities(replica.self).map(_._1)
    val gained             = replica.eventGraph.capabilities(replica.self).exists((hash, _) => !capabilitiesBefore(hash))
    val requests           = if gained then requestDeltas(step.state, step.state.peerRoles.keys.toList) else Nil
    changed(before, step.state, sends ++ deltas ++ requests)
  }

  private def changed(before: State, after: State, effects: Seq[Effect[Msg, Out]]): Result =
    val state = after.store.replica.state
    Step(after, if state != before.store.replica.state then effects :+ Effect.Deliver(state) else effects)

  /** Ask `peers` for the deltas we do not have (they answer with those we may read). */
  private def requestDeltas(state: State, peers: List[Peer]): List[Effect[Msg, Out]] =
    val wanted = state.store.replica.deltasWithoutValue
    if wanted.isEmpty || peers.isEmpty then Nil else List(Effect.Send(peers, AuthzMessage.RequestDeltas(wanted)))

  /** The stored deltas of `hashes` that `peer` may read, as a single message (if any). */
  private def deltasFor(replica: AuthzReplica[RDT], hashes: List[Hash], peer: Peer): List[Effect[Msg, Out]] =
    identityOf(peer).toList.flatMap { reader =>
      val readable = replica.readableDeltas(reader, hashes.filter(replica.containsEvent))
      Option.when(readable.nonEmpty)(Effect.Send(List(peer), AuthzMessage.Deltas(readable)))
    }
}

object AuthzProtocol {

  /** Identity of authenticated peers, see `channels.connection.tls.Tls.authenticatedPeer`. */
  def identityOf(peer: Peer): Option[PublicIdentity] = peer match
      case Peer.Authenticated(uid) => Some(PublicIdentity(Uid.unwrap(uid)))
      case Peer.Claimed(_)         => None

  /** A replica that only knows the hash of the genesis event; it learns the event itself from its peers. */
  def initial[RDT: {Lattice, Bottom, JsonValueCodec, Permits, Decompose}](self: PublicIdentity, genesis: Hash)(using
      Verifier
  ): PlumtreeCore[AuthzStore[RDT]] =
    PlumtreeCore(AuthzStore(AuthzReplica.initial[RDT](self, genesis)))

  /** A [[channels.MessagingIO]] for an authorization-aware replica of `RDT`, identified by `identity`.
    * Its overlay identifies itself as [[channels.connection.Peer.Authenticated]], so it only talks to peers over authenticated connections
    * (e.g., `channels.connection.tls.Tls`), whose identity is the one capabilities are granted to.
    */
  def messagingIO[RDT: {Lattice, Bottom, JsonValueCodec, Permits, Decompose}](
      identity: PrivateIdentity,
      genesis: Hash,
      onStateChange: RDT => Unit,
      config: MessagingIO.Config = MessagingIO.Config(),
  )(using Verifier): MessagingIO[AuthzProtocol[RDT]] = {
    val uid       = Uid(identity.getPublic.id)
    val withOwnId = config.copy(overlay =
      config.overlay.orElse(Some(FullMeshOverlay(PeerConnectInfo(Peer.Authenticated(uid), Set.empty))))
    )
    new MessagingIO[AuthzProtocol[RDT]](using AuthzProtocol[RDT](identity), AuthzMessage.codec)(
      ReplicaId.predefined(identity.getPublic.id),
      initial[RDT](identity.getPublic, genesis),
      onStateChange,
      withOwnId,
    )
  }

  /** Application API. Local operations throw if not permitted. */
  extension [RDT](io: MessagingIO[AuthzProtocol[RDT]]) {
    def replica: AuthzReplica[RDT] = io.state.store.replica

    def receiveLocally(event: AuthzEvent): Unit = io.update(io.protocol.receiveLocally(_, event))

    def mutate(mutator: RDT => RDT): Unit = io.update(s => io.protocol.write(s, mutator(s.store.replica.state)))

    def mutate(mutator: RDT => RDT, capability: Hash): Unit =
      io.update(s => io.protocol.write(s, mutator(s.store.replica.state), capability))

    def delegate(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree): Unit =
      io.update(io.protocol.delegate(_, usedCapability, delegatee, read, write))

    def revoke(revokedCapability: Hash): Unit = io.update(io.protocol.revoke(_, revokedCapability))
  }
}
