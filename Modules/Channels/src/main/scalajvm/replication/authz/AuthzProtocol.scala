package replication.authz

import channels.MessagingIO
import channels.broadcast.BroadcastProtocol
import channels.broadcast.BroadcastProtocol.{Effect, Step, Timer}
import channels.connection.{Peer, PeerConnectInfo}
import channels.crypto.{PrivateIdentity, PublicIdentity}
import channels.overlay.FullMeshOverlay
import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonReader, JsonValueCodec, JsonWriter, readFromArray, writeToArray
}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, Decompose, Lattice, ReplicaId, Uid}
import rdts.filters.{Filter, PermissionTree}
import replication.JsoniterCodecsJvm.{ardtEventCodec, hashValueCodec}
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import replication.crypto.Commitment.RevealedValue
import replication.crypto.{Commitment, Hash, Signature}

import scala.annotation.tailrec
import scala.util.Try

/** Messages of the [[AuthzProtocol]]. */
enum AuthzMessage {

  /** Signed events, encoded exactly as they are hashed, in an order in which they can be received. */
  case Events(events: List[Array[Byte]])

  /** Revealed values of delta commitments, filtered for what the receiver may read. */
  case Deltas(deltas: List[AuthzMessage.DeltaValue])

  /** Ask for the given events (and the deltas the requester may read). */
  case Request(events: Set[Hash])

  /** Ask for everything, sent by replicas that do not even know the genesis event yet. */
  case Bootstrap
}

object AuthzMessage {
  final case class DeltaValue(event: Hash, value: RevealedValue)

  private given bytesCodec: JsonValueCodec[Array[Byte]] = new JsonValueCodec[Array[Byte]] {
    def decodeValue(in: JsonReader, default: Array[Byte]): Array[Byte] = in.readBase64AsBytes(default)
    def encodeValue(x: Array[Byte], out: JsonWriter): Unit             = out.writeBase64Val(x, true)
    def nullValue: Array[Byte]                                         = Array.emptyByteArray
  }

  given codec: JsonValueCodec[AuthzMessage] = JsonCodecMaker.make
}

/** Immutable state of an authorization-aware replica: the [[ArdtEventGraph]], the delta values this replica may read,
  * and the anti-entropy bookkeeping. Driven by a [[channels.MessagingIO]] through [[AuthzProtocol]].
  *
  * Only authenticated peers ([[Peer.Authenticated]]) take part: their uid is their [[PublicIdentity]], which is what
  * capabilities are granted to, and what deltas are filtered for.
  */
final case class AuthzReplica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    self: PublicIdentity,
    eventGraph: ArdtEventGraph[RDT],
    /** The delta values this replica accepted. */
    deltas: DeltaValueStore[RDT],
    state: RDT,
    /** Events that are waiting for missing predecessors, by their hash. */
    waitingEvents: Map[Hash, Array[Byte]],
    /** Delta values whose commitment event is not known yet. */
    waitingDeltas: Map[Hash, RevealedValue],
    /** Events we know to exist (as predecessors), but do not have. */
    missing: Set[Hash],
    /** Peers that sent events with missing predecessors, asked in turn on repair. */
    knowledgeable: Vector[Peer],
    peers: Set[Peer],
    repairScheduled: Boolean,
    /** State as of an event index, to rematerialize from after revocations that invalidate only later deltas. */
    snapshot: Option[AuthzReplica.Snapshot[RDT]],
) {
  import AuthzReplica.*

  type Result = Step[AuthzReplica[RDT], AuthzMessage, RDT]

  def heads: Set[Hash] = eventGraph.heads

  def delta(eventHash: Hash): Option[RDT] =
    eventGraph.events.get(eventHash).flatMap((_, index) => deltas.get(index)).map(_.delta)

  def activeCapabilitiesOf(identity: PublicIdentity): Set[(Hash, Capability)] =
    eventGraph.activeCapabilitiesOf(identity)

  def activeCapabilities: Map[PublicIdentity, Set[(Hash, Capability)]] = eventGraph.activeCapabilities

  def containsEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)

  def deltaValue(eventHash: Hash): Option[RevealedValue] =
    eventGraph.events.get(eventHash).flatMap((_, index) => deltas.get(index)).map(stored =>
      RevealedValue(writeToArray(stored.delta), stored.witness)
    )

  /** Remember the current state, see [[snapshot]]. */
  def createSnapshot: AuthzReplica[RDT] =
    copy(snapshot = Some(AuthzReplica.Snapshot(eventGraph.nextEventIndex - 1, state)))

  // ------------------------------------------------------------------------------------------------
  // direct input, e.g., for trusted events, replay, and tests
  // ------------------------------------------------------------------------------------------------

  /** Adds an event to the event graph.
    * @return Right(Some(hash)) if the event was added, Right(None) if it was already known, and Left(missing) if it
    *         depends on events that are missing locally (it is then kept until they arrive).
    * @throws IllegalArgumentException if the event is invalid
    */
  def receiveEvent(encodedEvent: Array[Byte]): (AuthzReplica[RDT], Either[Set[Hash], Option[Hash]]) = {
    val (next, outcome) = receiveEventChecked(encodedEvent)
    (next, if outcome.missing.nonEmpty then Left(outcome.missing) else Right(outcome.added))
  }

  /** Stores the delta value of a known event and merges it into the state.
    * @throws IllegalArgumentException if the value does not match the commitment, may not be read locally, or
    *                                  was not allowed to be written
    * @throws NoSuchElementException if the event is unknown
    */
  def receiveDelta(eventHash: Hash, value: RevealedValue): AuthzReplica[RDT] =
    receiveDeltaChecked(eventHash, value)._1

  /** The values of `hashes` that `reader` may read. */
  def readableDeltas(reader: PublicIdentity, hashes: Iterable[Hash]): List[AuthzMessage.DeltaValue] =
    hashes.iterator.flatMap { hash =>
      eventGraph.events.get(hash).flatMap((_, index) => deltas.get(index)).collect {
        case stored if Authorization.mayReadAssumingCommitmentHolds(reader, hash, stored.delta, eventGraph) =>
          AuthzMessage.DeltaValue(hash, RevealedValue(writeToArray(stored.delta), stored.witness))
      }
    }.toList

  // ------------------------------------------------------------------------------------------------
  // network
  // ------------------------------------------------------------------------------------------------

  def addPeer(peer: Peer): Result = identityOf(peer) match
      case None           => Step(this, Nil)
      case Some(identity) =>
        val next = copy(peers = peers + peer)
        val sync =
          if heads.isEmpty then List(Effect.Send(List(peer), AuthzMessage.Bootstrap))
          else eventsWithDeltas(heads, peer, identity)
        Step(next, sync)

  def removePeer(peer: Peer): Result =
    Step(copy(peers = peers - peer, knowledgeable = knowledgeable.filterNot(_ == peer)), Nil)

  def handleMessage(from: Peer, message: AuthzMessage): Result = identityOf(from) match
      case None           => Step(this, Nil)
      case Some(identity) =>
        message match
            case AuthzMessage.Events(events) =>
              val (received, missingNow) = events.foldLeft((Changes(this), Set.empty[Hash])) {
                case ((changes, missingAcc), encoded) =>
                  val (next, outcome) = changes.replica.receiveEncodedEvent(encoded)
                  (changes.withReplica(next, outcome.changed), missingAcc ++ outcome.missing)
              }
              val withMissing =
                if missingNow.isEmpty then received.replica
                else
                    val r = received.replica
                    r.copy(
                      missing = r.missing ++ missingNow.filterNot(r.eventGraph.events.contains),
                      knowledgeable = if r.knowledgeable.contains(from) then r.knowledgeable else r.knowledgeable :+ from
                    )
              withMissing.retryWaiting(received.copy(replica = withMissing)).result.scheduleRepairIfNeeded

            case AuthzMessage.Deltas(values) =>
              values.foldLeft(Changes(this)) { (changes, value) =>
                val (next, changed) = changes.replica.receiveNetworkDelta(value.event, value.value)
                changes.withReplica(next, changed)
              }.result

            case AuthzMessage.Request(hashes) =>
              Step(this, eventsWithDeltas(hashes.filter(eventGraph.events.contains), from, identity))

            case AuthzMessage.Bootstrap =>
              Step(this, eventsWithDeltas(eventGraph.events.keySet, from, identity))

  /** Ask one of the knowledgeable peers for the missing events, rotating through them. */
  def timerFired(timer: Timer): Result = timer match
      case Timer.Repair =>
        val unscheduled = copy(repairScheduled = false, knowledgeable = knowledgeable.filter(peers.contains))
        unscheduled.knowledgeable match
            case ask +: others if unscheduled.missing.nonEmpty =>
              Step(
                unscheduled.copy(knowledgeable = others :+ ask),
                List(Effect.Send(List(ask), AuthzMessage.Request(unscheduled.missing)))
              ).scheduleRepairIfNeeded
            case _ => Step(unscheduled, Nil)

  // ------------------------------------------------------------------------------------------------
  // local operations
  // ------------------------------------------------------------------------------------------------

  /** Write `delta` using any active capability of `identity` that allows it. */
  def write(identity: PrivateIdentity, delta: RDT): Result =
    eventGraph.capabilities(identity.getPublic).find { (hash, capability) =>
      Filter[RDT].isAllowed(delta, capability.write) && eventGraph.revocations(hash).isEmpty
    } match
        case Some((hash, _)) => write(identity, delta, hash)
        case None            => throw IllegalArgumentException(s"Insufficient permissions for mutation: $delta")

  /** Write `delta` using the capability `capabilityHash`. Decomposed deltas become a chain of events. */
  def write(identity: PrivateIdentity, delta: RDT, capabilityHash: Hash): Result = {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    require(eventGraph.events.get(capabilityHash) match
        case Some((ArdtEvent(Capability(holder, _, write), _, _, _, _), _)) =>
          holder == identity.getPublic && Filter[RDT].isAllowed(delta, write)
        case _ => false)

    val (next, created) = Decompose.decompose(delta).foldLeft((this, List.empty[(Hash, Array[Byte])])) {
      case ((replica, created), part) =>
        val revealed = Commitment.commit(identity.getPublic.id, writeToArray(part))
        val encoded  = writeToArray(replica.createSignedEvent(
          identity,
          DeltaCommitment(revealed.commitment(identity.getPublic.id)),
          capabilityHash,
          replica.heads
        ))
        val (withEvent, outcome) = replica.receiveEventChecked(encoded)
        require(outcome.added.nonEmpty, s"local event was not accepted: $outcome")
        val hash = outcome.added.get
        (withEvent.storeDelta(hash, part, revealed.witness), (hash, encoded) :: created)
    }
    val events = created.reverse
    Step(
      next,
      next.disseminate(events.map(_._2)) ++ next.peers.toList.flatMap(peer =>
        next.deltasFor(events.map(_._1), peer)
      ) :+ Effect.Deliver(next.state)
    )
  }

  /** Delegate a capability derived from `usedCapability` (held by `identity`) to `delegatee`. */
  def delegate(
      identity: PrivateIdentity,
      usedCapability: Hash,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): Result = {
    require(write <= read)
    eventGraph.events.get(usedCapability) match
        case Some((ArdtEvent(Capability(holder, readLimit, writeLimit), _, _, _, _), _)) =>
          require(holder == identity.getPublic)
          require(read <= readLimit && write <= writeLimit)
          localEvent(identity, Capability(delegatee, read, write), usedCapability)
        case _ => throw IllegalArgumentException("Referenced capability event is not a capability")
  }

  /** Revoke `revokedCapability`, authorized by the closest capability held by `identity` in its authorization chain. */
  def revoke(identity: PrivateIdentity, revokedCapability: Hash): Result = {
    @tailrec
    def authorizationFor(authEvent: Hash): Option[Hash] =
      if authEvent == Hash.allZeroHash then None
      else
          eventGraph.events(authEvent)._1 match
              case ArdtEvent(Capability(holder, _, _), _, _, _, _) if holder == identity.getPublic => Some(authEvent)
              case ArdtEvent(_, _, _, _, parent)                                               => authorizationFor(parent)

    authorizationFor(revokedCapability) match
        case Some(authorization) => localEvent(identity, Revocation(revokedCapability), authorization)
        case None                =>
          throw IllegalStateException("No capability in authorization chain found to perform revocation")
  }

  /** Accept a (trusted) event out of band, e.g., the genesis event. */
  def receiveLocally(event: ArdtEvent): Result = {
    val (next, outcome) = receiveEventChecked(writeToArray(event))
    require(outcome.missing.isEmpty, s"missing predecessors: ${outcome.missing}")
    Step(next, if outcome.changed then List(Effect.Deliver(next.state)) else Nil)
  }

  // ------------------------------------------------------------------------------------------------
  // helpers
  // ------------------------------------------------------------------------------------------------

  private def localEvent(identity: PrivateIdentity, payload: ArdtEvent.Payload, authorization: Hash): Result = {
    val encoded         = writeToArray(createSignedEvent(identity, payload, authorization, heads))
    val (next, outcome) = receiveEventChecked(encoded)
    require(outcome.added.nonEmpty, s"local event was not accepted: $outcome")
    Step(next, next.disseminate(List(encoded)) ++ Option.when(outcome.changed)(Effect.Deliver(next.state)))
  }

  private def createSignedEvent(
      identity: PrivateIdentity,
      payload: ArdtEvent.Payload,
      capability: Hash,
      parents: Set[Hash]
  ): ArdtEvent = {
    val unsigned = ArdtEvent(payload, identity.getPublic, parents, Signature.allZeroSignature, capability)
    unsigned.copy(signature = Signature.compute(writeToArray(unsigned), identity.identityKey.getPrivate))
  }

  private def disseminate(events: List[Array[Byte]]): List[Effect[AuthzMessage, Nothing]] =
    if peers.isEmpty || events.isEmpty then Nil
    else List(Effect.Send(peers.toList, AuthzMessage.Events(events)))

  /** The events `hashes` (in causal order) and the deltas of them that `peer` may read. */
  private def eventsWithDeltas(
      hashes: Set[Hash],
      peer: Peer,
      identity: PublicIdentity
  ): List[Effect[AuthzMessage, Nothing]] = {
    val ordered = hashes.toList.sortBy(eventGraph.events(_)._2)
    if ordered.isEmpty then Nil
    else
        Effect.Send(List(peer), AuthzMessage.Events(ordered.map(h => writeToArray(eventGraph.events(h)._1))))
        :: deltasFor(ordered, peer)
  }

  /** The stored deltas of `hashes` that `peer` may read, as a single message (if any). */
  private def deltasFor(hashes: List[Hash], peer: Peer): List[Effect[AuthzMessage, Nothing]] =
    identityOf(peer).toList.flatMap { reader =>
      val readable = readableDeltas(reader, hashes)
      Option.when(readable.nonEmpty)(Effect.Send(List(peer), AuthzMessage.Deltas(readable)))
    }

  /** Adds an event to the graph, dropping invalid ones, as needed for events received from the network. */
  private def receiveEncodedEvent(encoded: Array[Byte]): (AuthzReplica[RDT], EventOutcome) =
    Try(receiveEventChecked(encoded)).getOrElse((this, EventOutcome()))

  /** Adds an event to the graph. Revocations may invalidate stored deltas.
    * @throws IllegalArgumentException if the event is invalid
    */
  private def receiveEventChecked(encoded: Array[Byte]): (AuthzReplica[RDT], EventOutcome) =
    eventGraph.receive(encoded) match
        case Left(missingPredecessors) =>
          val hash = Hash.compute(encoded)
          (copy(waitingEvents = waitingEvents.updated(hash, encoded)), EventOutcome(missing = missingPredecessors))
        case Right(graph) if graph eq eventGraph => (this, EventOutcome())
        case Right(graph)                        =>
          val hash     = Hash.compute(encoded)
          val accepted = copy(
            eventGraph = graph,
            missing = missing - hash,
            waitingEvents = waitingEvents - hash,
          )
          val (afterRevocation, invalidated) = graph.events(hash)._1.payload match
              // if the revocation is the only head, every event is in its causal past
              case Revocation(_) if graph.heads.size > 1 => accepted.invalidateDeltas(hash)
              case _                                     => (accepted, false)
          // the delta of this event may have arrived before it
          val (afterDelta, deltaApplied) = afterRevocation.waitingDeltas.get(hash) match
              case Some(value) =>
                afterRevocation.copy(waitingDeltas = afterRevocation.waitingDeltas - hash).receiveNetworkDelta(hash, value)
              case None => (afterRevocation, false)
          (afterDelta, EventOutcome(added = Some(hash), changed = invalidated || deltaApplied))

  /** Retry events waiting for predecessors until no more can be added. */
  @tailrec
  private def retryWaiting(changes: Changes[RDT]): Changes[RDT] = {
    val replica         = changes.replica
    val (next, anyAdded) = replica.waitingEvents.values.foldLeft((changes, false)) {
      case ((acc, added), encoded) =>
        val (r, outcome) = acc.replica.receiveEncodedEvent(encoded)
        (acc.withReplica(r, outcome.changed), added || outcome.added.nonEmpty)
    }
    if anyAdded then next.replica.retryWaiting(next) else next
  }

  /** Removes the deltas the revocation invalidates and rematerializes, keeping the snapshot if it is unaffected. */
  private def invalidateDeltas(revocation: Hash): (AuthzReplica[RDT], Boolean) = {
    val invalidated = eventGraph.deltasInvalidatedBy(revocation).filter(deltas.contains).toSet
    if invalidated.isEmpty then (this, false)
    else
        val keptSnapshot = snapshot.filterNot(s => invalidated.exists(_ <= s.version))
        val next         = copy(deltas = deltas.removedAll(invalidated), snapshot = keptSnapshot)
        (next.copy(state = next.rematerialized), true)
  }

  /** The state from the snapshot (if any) and the deltas of events after it. */
  private def rematerialized: RDT = snapshot match
      case None           => deltas.merged
      case Some(snapshot) =>
        // walk back from the heads, through the events after the snapshot
        val toVisit = scala.collection.mutable.Queue.from(eventGraph.heads)
        val visited = scala.collection.mutable.Set.from(eventGraph.heads)
        var result  = snapshot.state
        while toVisit.nonEmpty do {
          val (event, index) = eventGraph.events(toVisit.dequeue())
          if index > snapshot.version then
              deltas.get(index).foreach(stored => result = result.merge(stored.delta))
              val parents = event.parents.diff(visited)
              toVisit.enqueueAll(parents)
              visited.addAll(parents)
        }
        result

  /** Accepts a delta value from the network: buffered if its event is unknown, dropped if it is not acceptable. */
  private def receiveNetworkDelta(eventHash: Hash, value: RevealedValue): (AuthzReplica[RDT], Boolean) =
    if !eventGraph.events.contains(eventHash) then (copy(waitingDeltas = waitingDeltas.updated(eventHash, value)), false)
    else Try(receiveDeltaChecked(eventHash, value)).getOrElse((this, false))

  /** Accepts a delta value of a known event, if its commitment holds, it may be read here and was allowed to be written.
    * @throws IllegalArgumentException otherwise
    */
  private def receiveDeltaChecked(eventHash: Hash, value: RevealedValue): (AuthzReplica[RDT], Boolean) = {
    val (event, index) = eventGraph.events(eventHash)
    if deltas.contains(index) then (this, false)
    else
        val commitment = value.commitment(event.author.id)
        require(event.payload == DeltaCommitment(commitment), "commitment does not hold")
        val delta = readFromArray[RDT](value.value)
        require(Authorization.mayReadAssumingCommitmentHolds(self, eventHash, delta, eventGraph), "may not read")
        require(Authorization.mayWriteAssumingCommitmentHolds(eventGraph, eventHash, event, delta), "may not write")
        (storeDelta(eventHash, delta, value.witness), true)
  }

  private def storeDelta(eventHash: Hash, delta: RDT, witness: Array[Byte]): AuthzReplica[RDT] = {
    val index = eventGraph.events(eventHash)._2
    copy(
      deltas = deltas.put(index, delta, witness),
      state = state.merge(delta),
      snapshot = snapshot.map(s => if index <= s.version then s.copy(state = s.state.merge(delta)) else s),
    )
  }

  extension (step: Result)
    private def scheduleRepairIfNeeded: Result =
      val replica = step.state
      if replica.repairScheduled || replica.missing.isEmpty then step
      else Step(replica.copy(repairScheduled = true), step.events :+ Effect.StartTimer(Timer.Repair))
}

object AuthzReplica {

  /** A replica that only knows the hash of the genesis event; it learns the event itself from its peers. */
  def initial[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
      self: PublicIdentity,
      genesis: Hash
  ): AuthzReplica[RDT] =
    AuthzReplica[RDT](
      self,
      ArdtEventGraph[RDT](genesis),
      deltas = DeltaValueStore(),
      state = Bottom[RDT].empty,
      waitingEvents = Map.empty,
      waitingDeltas = Map.empty,
      missing = Set.empty,
      knowledgeable = Vector.empty,
      peers = Set.empty,
      repairScheduled = false,
      snapshot = None,
    )

  /** Identity of authenticated peers, see [[channels.NioTls.authenticatedPeer]]. */
  def identityOf(peer: Peer): Option[PublicIdentity] = peer match
      case Peer.Authenticated(uid) => Some(PublicIdentity(Uid.unwrap(uid)))
      case Peer.Claimed(_)         => None

  final case class Snapshot[RDT](version: Int, state: RDT)

  final private[authz] case class EventOutcome(
      added: Option[Hash] = None,
      missing: Set[Hash] = Set.empty,
      changed: Boolean = false,
  )

  /** Accumulates whether the materialized state changed during a transition. */
  final private[authz] case class Changes[RDT](replica: AuthzReplica[RDT], changed: Boolean = false) {
    def withReplica(next: AuthzReplica[RDT], changedNow: Boolean): Changes[RDT] =
      Changes(next, changed || changedNow)
    def result: Step[AuthzReplica[RDT], AuthzMessage, RDT] =
      Step(replica, if changed then List(Effect.Deliver(replica.state)) else Nil)
  }
}

/** [[BroadcastProtocol]] instance for [[AuthzReplica]]. Delivers the materialized state whenever it changes.
  * `identity` signs local events, see the extension methods in the companion.
  */
final class AuthzProtocol[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](val identity: PrivateIdentity)
    extends BroadcastProtocol {
  type State = AuthzReplica[RDT]
  type Msg   = AuthzMessage
  type Out   = RDT

  def addPeer(state: State, peer: Peer): state.Result                         = state.addPeer(peer)
  def removePeer(state: State, peer: Peer): state.Result                      = state.removePeer(peer)
  def handleMessage(state: State, from: Peer, message: AuthzMessage): state.Result = state.handleMessage(from, message)
  def timerFired(state: State, timer: Timer): state.Result                    = state.timerFired(timer)
}

object AuthzProtocol {

  /** A [[MessagingIO]] for an authorization-aware replica of `RDT`, identified by `identity`.
    * Its overlay identifies itself as [[Peer.Authenticated]], so it only talks to peers over authenticated connections
    * (e.g., [[channels.NioTCP.listenTls]]), whose identity is the one capabilities are granted to.
    */
  def messagingIO[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
      identity: PrivateIdentity,
      genesis: Hash,
      onStateChange: RDT => Unit,
      config: MessagingIO.Config = MessagingIO.Config(),
  ): MessagingIO[AuthzProtocol[RDT]] = {
    val uid       = Uid(identity.getPublic.id)
    val withOwnId = config.copy(overlay =
      config.overlay.orElse(Some(FullMeshOverlay(PeerConnectInfo(Peer.Authenticated(uid), Set.empty))))
    )
    new MessagingIO[AuthzProtocol[RDT]](using AuthzProtocol[RDT](identity), AuthzMessage.codec)(
      ReplicaId.predefined(identity.getPublic.id),
      AuthzReplica.initial[RDT](identity.getPublic, genesis),
      onStateChange,
      withOwnId,
    )
  }

  /** Application API. Local operations throw if not permitted. */
  extension [RDT](io: MessagingIO[AuthzProtocol[RDT]]) {
    private def identity: PrivateIdentity = io.protocol.identity

    def replica: AuthzReplica[RDT] = io.state

    def receiveLocally(event: ArdtEvent): Unit = io.update(_.receiveLocally(event))

    def mutate(mutator: RDT => RDT): Unit = io.update(r => r.write(identity, mutator(r.state)))

    def mutate(mutator: RDT => RDT, capability: Hash): Unit =
      io.update(r => r.write(identity, mutator(r.state), capability))

    def delegate(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree): Unit =
      io.update(_.delegate(identity, usedCapability, delegatee, read, write))

    def revoke(revokedCapability: Hash): Unit = io.update(_.revoke(identity, revokedCapability))
  }
}
