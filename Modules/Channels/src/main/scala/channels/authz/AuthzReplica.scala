package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Commitment, Hash, PrivateIdentity, PublicIdentity, Signature, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.{Bottom, Decompose, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}

import scala.annotation.tailrec
import scala.util.Try

object AuthzReplica {

  /** A replica that only knows the hash of the genesis event; it learns the event itself from its peers. */
  def initial[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
      self: PublicIdentity,
      genesis: Hash
  )(using Verifier): AuthzReplica[RDT] =
    AuthzReplica[RDT](
      self,
      AuthzEventGraph(genesis),
      deltas = DeltaValueStore(),
      state = Bottom[RDT].empty,
      waitingEvents = Map.empty,
      waitingDeltas = Map.empty,
      snapshot = None,
    )

  final case class Snapshot[RDT](version: Int, state: RDT)

  /** The events that must be in the graph before `event` can be added. */
  def dependencies(event: AuthzEvent): Set[Hash] =
    val authorization = Option.when(event.authorization != Hash.allZeroHash)(event.authorization)
    event.payload match
        case Revocation(revoked) => event.parents ++ authorization + revoked
        case _                   => event.parents ++ authorization

  final private[authz] case class EventOutcome(
      added: Option[Hash] = None,
      missing: Set[Hash] = Set.empty,
      changed: Boolean = false,
  )

  /** Result of a local operation: the next replica, and the (encoded) events it created. */
  final case class Local[RDT](state: AuthzReplica[RDT], events: List[Array[Byte]])
}

/** Immutable state of an authorization-aware replica: the [[AuthzEventGraph]], the events waiting for missing
  * predecessors, and the delta values this replica may read. Disseminated by [[channels.routing.authz.AuthzProtocol]].
  */
final case class AuthzReplica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    self: PublicIdentity,
    eventGraph: AuthzEventGraph,
    /** The delta values this replica accepted. */
    deltas: DeltaValueStore[RDT],
    state: RDT,
    /** Events that are waiting for missing predecessors, by their hash. */
    waitingEvents: Map[Hash, Array[Byte]],
    /** Delta values whose commitment event is not known yet. */
    waitingDeltas: Map[Hash, RevealedValue],
    /** State as of an event index, to rematerialize from after revocations that invalidate only later deltas. */
    snapshot: Option[AuthzReplica.Snapshot[RDT]],
)(using Verifier) {
  import AuthzReplica.*

  def heads: Set[Hash] = eventGraph.heads

  def delta(eventHash: Hash): Option[RDT] =
    eventGraph.events.get(eventHash).flatMap((_, index) => deltas.get(index)).map(_.delta)

  def activeCapabilitiesOf(identity: PublicIdentity): Set[(Hash, Capability)] =
    eventGraph.activeCapabilitiesOf(identity)

  def activeCapabilities: Map[PublicIdentity, Set[(Hash, Capability)]] = eventGraph.activeCapabilities

  def containsEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)

  /** Contained or waiting for predecessors. */
  def received(eventHash: Hash): Boolean = containsEvent(eventHash) || waitingEvents.contains(eventHash)

  /** Events we know to exist (as predecessors of waiting events), but do not have. */
  def missing: Set[Hash] =
    waitingEvents.values.flatMap(encoded => AuthzReplica.dependencies(readFromArray[AuthzEvent](encoded)))
      .filterNot(received).toSet

  /** The events not in the causal past of `hashes`, encoded, in an order in which they can be received. */
  def notBefore(hashes: Set[Hash]): List[Array[Byte]] =
    eventGraph.notBefore(hashes).map(hash => writeToArray(eventGraph.events(hash)._1))

  /** Known delta commitment events without a (pending) value, e.g., because they were not readable when received. */
  def deltasWithoutValue: Set[Hash] =
    eventGraph.events.iterator.collect {
      case (hash, (AuthzEvent(DeltaCommitment(_), _, _, _, _), index))
          if !deltas.contains(index) && !waitingDeltas.contains(hash) => hash
    }.toSet

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
  def readableDeltas(reader: PublicIdentity, hashes: Iterable[Hash]): List[DeltaValue] =
    hashes.iterator.flatMap { hash =>
      eventGraph.events.get(hash).flatMap((_, index) => deltas.get(index)).collect {
        case stored if Authorization.mayReadAssumingCommitmentHolds(reader, hash, stored.delta, eventGraph) =>
          DeltaValue(hash, RevealedValue(writeToArray(stored.delta), stored.witness))
      }
    }.toList

  /** Adds an event received from the network, keeping it until its predecessors arrive, and dropping it if invalid.
    * @return the events that were added to the graph because of it, encoded, in the order they were added
    */
  def receiveEncoded(encoded: Array[Byte]): (AuthzReplica[RDT], List[Array[Byte]]) = {
    val (next, outcome) = receiveEncodedEvent(encoded)
    if outcome.added.isEmpty then (next, Nil)
    else
        val (retried, added) = next.retryWaiting()
        (retried, encoded :: added)
  }

  /** Accepts delta values from the network: buffered if their event is unknown, dropped if not acceptable. */
  def receiveDeltas(values: List[DeltaValue]): AuthzReplica[RDT] =
    values.foldLeft(this)((replica, value) => replica.receiveNetworkDelta(value.event, value.value)._1)

  // ------------------------------------------------------------------------------------------------
  // local operations
  // ------------------------------------------------------------------------------------------------

  /** Write `delta` using any active capability of `identity` that allows it. */
  def write(identity: PrivateIdentity, delta: RDT): Local[RDT] =
    eventGraph.capabilities(identity.getPublic).find { (hash, capability) =>
      Filter[RDT].isAllowed(delta, capability.write) && eventGraph.revocations(hash).isEmpty
    } match
        case Some((hash, _)) => write(identity, delta, hash)
        case None            => throw IllegalArgumentException(s"Insufficient permissions for mutation: $delta")

  /** Write `delta` using the capability `capabilityHash`. Decomposed deltas become a chain of events. */
  def write(identity: PrivateIdentity, delta: RDT, capabilityHash: Hash): Local[RDT] = {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    require(eventGraph.events.get(capabilityHash) match
        case Some((AuthzEvent(Capability(holder, _, write), _, _, _, _), _)) =>
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
    Local(next, created.reverse.map(_._2))
  }

  /** Delegate a capability derived from `usedCapability` (held by `identity`) to `delegatee`. */
  def delegate(
      identity: PrivateIdentity,
      usedCapability: Hash,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): Local[RDT] = {
    require(write <= read)
    eventGraph.events.get(usedCapability) match
        case Some((AuthzEvent(Capability(holder, readLimit, writeLimit), _, _, _, _), _)) =>
          require(holder == identity.getPublic)
          require(read <= readLimit && write <= writeLimit)
          localEvent(identity, Capability(delegatee, read, write), usedCapability)
        case _ => throw IllegalArgumentException("Referenced capability event is not a capability")
  }

  /** Revoke `revokedCapability`, authorized by the closest capability held by `identity` in its authorization chain. */
  def revoke(identity: PrivateIdentity, revokedCapability: Hash): Local[RDT] = {
    @tailrec
    def authorizationFor(authEvent: Hash): Option[Hash] =
      if authEvent == Hash.allZeroHash then None
      else
          eventGraph.events(authEvent)._1 match
              case AuthzEvent(Capability(holder, _, _), _, _, _, _) if holder == identity.getPublic => Some(authEvent)
              case AuthzEvent(_, _, _, _, parent) => authorizationFor(parent)

    authorizationFor(revokedCapability) match
        case Some(authorization) => localEvent(identity, Revocation(revokedCapability), authorization)
        case None                =>
          throw IllegalStateException("No capability in authorization chain found to perform revocation")
  }

  /** Accept a (trusted) event out of band, e.g., the genesis event. */
  def receiveLocally(event: AuthzEvent): Local[RDT] = {
    val encoded         = writeToArray(event)
    val (next, outcome) = receiveEventChecked(encoded)
    require(outcome.missing.isEmpty, s"missing predecessors: ${outcome.missing}")
    Local(next, outcome.added.map(_ => encoded).toList)
  }

  // ------------------------------------------------------------------------------------------------
  // helpers
  // ------------------------------------------------------------------------------------------------

  private def localEvent(identity: PrivateIdentity, payload: AuthzEvent.Payload, authorization: Hash): Local[RDT] = {
    val encoded         = writeToArray(createSignedEvent(identity, payload, authorization, heads))
    val (next, outcome) = receiveEventChecked(encoded)
    require(outcome.added.nonEmpty, s"local event was not accepted: $outcome")
    Local(next, List(encoded))
  }

  private def createSignedEvent(
      identity: PrivateIdentity,
      payload: AuthzEvent.Payload,
      capability: Hash,
      parents: Set[Hash]
  ): AuthzEvent = {
    val unsigned = AuthzEvent(payload, identity.getPublic, parents, Signature.allZeroSignature, capability)
    unsigned.copy(signature = identity.sign(writeToArray(unsigned)))
  }

  /** Adds an event to the graph, dropping invalid ones, as needed for events received from the network. */
  private def receiveEncodedEvent(encoded: Array[Byte]): (AuthzReplica[RDT], EventOutcome) =
    Try(receiveEventChecked(encoded)).getOrElse((copy(waitingEvents = waitingEvents - Hash.compute(encoded)), EventOutcome()))

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
          val accepted = copy(eventGraph = graph, waitingEvents = waitingEvents - hash)
          val (afterRevocation, invalidated) = graph.events(hash)._1.payload match
              // if the revocation is the only head, every event is in its causal past
              case Revocation(_) if graph.heads.size > 1 => accepted.invalidateDeltas(hash)
              case _                                     => (accepted, false)
          // the delta of this event may have arrived before it
          val (afterDelta, deltaApplied) = afterRevocation.waitingDeltas.get(hash) match
              case Some(value) =>
                afterRevocation.copy(waitingDeltas =
                  afterRevocation.waitingDeltas - hash
                ).receiveNetworkDelta(hash, value)
              case None => (afterRevocation, false)
          (afterDelta, EventOutcome(added = Some(hash), changed = invalidated || deltaApplied))

  /** Retry events waiting for predecessors until no more can be added.
    * @return the events that were added, encoded, in the order they were added
    */
  @tailrec
  private def retryWaiting(added: List[Array[Byte]] = Nil): (AuthzReplica[RDT], List[Array[Byte]]) = {
    val (next, addedNow) = waitingEvents.values.foldLeft((this, List.empty[Array[Byte]])) {
      case ((replica, acc), encoded) =>
        val (r, outcome) = replica.receiveEncodedEvent(encoded)
        (r, if outcome.added.nonEmpty then encoded :: acc else acc)
    }
    if addedNow.isEmpty then (next, added) else next.retryWaiting(added ++ addedNow.reverse)
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
    if !eventGraph.events.contains(eventHash) then
        (copy(waitingDeltas = waitingDeltas.updated(eventHash, value)), false)
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
}
