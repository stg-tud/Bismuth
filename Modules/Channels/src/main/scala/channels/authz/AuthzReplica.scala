package channels.authz

import AuthzEvent.Payload.{Capability, Revocation}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Hash, PrivateIdentity, PublicIdentity, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Permits, PermissionTree}

object AuthzReplica {

  /** A replica that only knows the hash of the genesis event; it learns the event itself from its peers. */
  def initial[RDT: {Lattice, Bottom, JsonValueCodec, Permits, Decompose}](
      self: PublicIdentity,
      genesis: Hash
  )(using Verifier): AuthzReplica[RDT] =
    AuthzReplica[RDT](
      self,
      AuthzEventGraph(genesis),
      deltas = AuthzDeltas.empty,
    )

  /** Result of a local operation: the next replica, and the (encoded) events it created. */
  final case class Local[RDT](state: AuthzReplica[RDT], events: List[Array[Byte]])
}

/** Immutable state of an authorization-aware replica: the [[AuthzEventGraph]] and the delta values this replica may
  * read ([[AuthzDeltas]]). Disseminated by [[channels.routing.authz.AuthzProtocol]].
  */
final case class AuthzReplica[RDT: {Lattice, Bottom, JsonValueCodec, Permits, Decompose}](
    self: PublicIdentity,
    eventGraph: AuthzEventGraph,
    deltas: AuthzDeltas[RDT],
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
  def received(eventHash: Hash): Boolean = eventGraph.received(eventHash)

  /** Events we know to exist (as predecessors of waiting events), but do not have. */
  def missing: Set[Hash] = eventGraph.missing

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
    val (graph, result) = eventGraph.receiveOrWait(encodedEvent)
    (withGraph(graph, result), result)
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
    val (graph, nextDeltas, hash) = eventGraph.receiveOrDrop(encoded, deltas)(reactTo)
    if hash.isEmpty then (copy(eventGraph = graph, deltas = nextDeltas), Nil)
    else
        val (retriedGraph, retriedDeltas, added) = graph.retryWaiting(nextDeltas)(reactTo)
        (copy(eventGraph = retriedGraph, deltas = retriedDeltas), encoded :: added)
  }

  /** Accepts delta values from the network: buffered if their event is unknown, dropped if not acceptable. */
  def receiveDeltas(values: List[DeltaValue]): AuthzReplica[RDT] =
    copy(deltas = values.foldLeft(deltas)((deltas, value) => deltas.receive(eventGraph, self, value.event, value.value)._1))

  // ------------------------------------------------------------------------------------------------
  // local operations
  // ------------------------------------------------------------------------------------------------

  /** Write `delta` using any active capability of `identity` that allows it. */
  def write(identity: PrivateIdentity, delta: RDT): Local[RDT] =
    write(identity, delta, AuthzAuthoring.writeCapability(eventGraph, identity.getPublic, delta))

  /** Write `delta` using the capability `capabilityHash`, see [[AuthzAuthoring.write]]. */
  def write(identity: PrivateIdentity, delta: RDT, capabilityHash: Hash): Local[RDT] =
    AuthzAuthoring.write(eventGraph, identity, delta, capabilityHash).foldLeft(Local(this, Nil)) { (local, created) =>
      val added = local.state.receiveCreated(created.event)
      Local(added.state.receiveDelta(created.event.hash, created.value), local.events ++ added.events)
    }

  /** See [[CapabilityPolicy.delegate]]. */
  def delegate(
      identity: PrivateIdentity,
      usedCapability: Hash,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): Local[RDT] =
    receiveCreated(eventGraph.policy.delegate(eventGraph.dag, identity, usedCapability, delegatee, read, write))

  /** See [[CapabilityPolicy.revoke]]. */
  def revoke(identity: PrivateIdentity, revokedCapability: Hash): Local[RDT] =
    receiveCreated(eventGraph.policy.revoke(eventGraph.dag, identity, revokedCapability))

  /** Accept a (trusted) event out of band, e.g., the genesis event. */
  def receiveLocally(event: AuthzEvent): Local[RDT] = {
    val encoded         = writeToArray(event)
    val (graph, result) = eventGraph.receiveOrWait(encoded)
    require(result.isRight, s"missing predecessors: ${result.left.getOrElse(Set.empty)}")
    Local(withGraph(graph, result), result.toOption.flatten.map(_ => encoded).toList)
  }

  // ------------------------------------------------------------------------------------------------
  // helpers
  // ------------------------------------------------------------------------------------------------

  /** Adds an event created by [[AuthzAuthoring]]. */
  private def receiveCreated(event: AuthzEvent): Local[RDT] = {
    val local = receiveLocally(event)
    require(local.events.nonEmpty, s"local event was not accepted: $event")
    local
  }

  /** The replica with `graph`, after reacting to the event that `result` reports as added, if any. */
  private def withGraph(graph: AuthzEventGraph, result: Either[Set[Hash], Option[Hash]]): AuthzReplica[RDT] =
    result match
        case Right(Some(hash)) => copy(eventGraph = graph, deltas = reactTo(deltas, graph, hash))
        case _                 => copy(eventGraph = graph)

  /** Updates the deltas for the event `hash` that was just added to `graph`: revocations may invalidate stored deltas,
    * and the delta of the event may have arrived before it.
    */
  private def reactTo(deltas: AuthzDeltas[RDT], graph: AuthzEventGraph, hash: Hash): AuthzDeltas[RDT] = {
    val afterRevocation = graph.events(hash)._1.payload match
        // if the revocation is the only head, every event is in its causal past
        case Revocation(_) if graph.heads.size > 1 => deltas.invalidate(graph, hash)._1
        case _                                     => deltas
    afterRevocation.receiveWaiting(graph, self, hash)._1
  }
}
