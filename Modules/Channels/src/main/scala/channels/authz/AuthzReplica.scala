package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Commitment, Hash, PrivateIdentity, PublicIdentity, Signature, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.{Bottom, Decompose, Lattice}
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
      deltas = AuthzDeltas.empty,
      waitingEvents = Map.empty,
    )

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
  * predecessors, and the delta values this replica may read ([[AuthzDeltas]]). Disseminated by [[channels.routing.authz.AuthzProtocol]].
  */
final case class AuthzReplica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    self: PublicIdentity,
    eventGraph: AuthzEventGraph,
    deltas: AuthzDeltas[RDT],
    /** Events that are waiting for missing predecessors, by their hash. */
    waitingEvents: Map[Hash, Array[Byte]],
)(using Verifier) {
  import AuthzReplica.*

  def heads: Set[Hash] = eventGraph.heads

  /** The materialized state. */
  def state: RDT = deltas.state

  def delta(eventHash: Hash): Option[RDT] = deltas.delta(eventGraph, eventHash)

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
  def deltasWithoutValue: Set[Hash] = deltas.withoutValue(eventGraph)

  def deltaValue(eventHash: Hash): Option[RevealedValue] = deltas.deltaValue(eventGraph, eventHash)

  /** Remember the current state, see [[AuthzDeltas.snapshot]]. */
  def createSnapshot: AuthzReplica[RDT] = copy(deltas = deltas.createSnapshot(eventGraph))

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
    copy(deltas = deltas.receiveChecked(eventGraph, self, eventHash, value)._1)

  /** The values of `hashes` that `reader` may read. */
  def readableDeltas(reader: PublicIdentity, hashes: Iterable[Hash]): List[DeltaValue] =
    deltas.readableBy(eventGraph, reader, hashes)

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
    copy(deltas = values.foldLeft(deltas)((deltas, value) => deltas.receive(eventGraph, self, value.event, value.value)._1))

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
        val stored = withEvent.copy(deltas = withEvent.deltas.put(withEvent.eventGraph, hash, part, revealed.witness))
        (stored, (hash, encoded) :: created)
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
              case Revocation(_) if graph.heads.size > 1 => deltas.invalidate(graph, hash)
              case _                                     => (deltas, false)
          // the delta of this event may have arrived before it
          val (afterDelta, deltaApplied) = afterRevocation.receiveWaiting(graph, self, hash)
          (accepted.copy(deltas = afterDelta), EventOutcome(added = Some(hash), changed = invalidated || deltaApplied))

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
}
