package channels.authz

import AuthzEvent.Payload.DeltaCommitment
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Hash, PublicIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.{Bottom, Lattice}
import rdts.filters.Permits

import scala.util.Try

object AuthzDeltas {

  def empty[RDT: {Lattice, Bottom, JsonValueCodec, Permits}]: AuthzDeltas[RDT] =
    AuthzDeltas(DeltaValueStore(), Bottom[RDT].empty, Map.empty, None)

  /** State as of an event index, to rematerialize from after revocations that invalidate only later deltas. */
  final case class Snapshot[RDT](version: Int, state: RDT)
}

/** The delta values of an [[AuthzReplica]] and the state they materialize to. Immutable.
  *
  * Knows nothing about events itself: operations take the [[AuthzEventGraph]] the values belong to, which decides
  * whether a value is acceptable, and which values a revocation invalidates.
  */
final case class AuthzDeltas[RDT: {Lattice, Bottom, JsonValueCodec, Permits}](
    /** The delta values this replica accepted. */
    store: DeltaValueStore[RDT],
    /** The merge of the accepted delta values. */
    state: RDT,
    /** Delta values whose commitment event is not known yet. */
    waiting: Map[Hash, RevealedValue],
    snapshot: Option[AuthzDeltas.Snapshot[RDT]],
) {

  def delta(graph: AuthzEventGraph, eventHash: Hash): Option[RDT] =
    graph.events.get(eventHash).flatMap((_, index) => store.get(index)).map(_.delta)

  def deltaValue(graph: AuthzEventGraph, eventHash: Hash): Option[RevealedValue] =
    graph.events.get(eventHash).flatMap((_, index) => store.getRevealedValue(index))

  /** Known delta commitment events without a (pending) value, e.g., because they were not readable when received. */
  def withoutValue(graph: AuthzEventGraph): Set[Hash] =
    graph.events.iterator.collect {
      case (hash, (AuthzEvent(DeltaCommitment(_), _, _, _, _), index))
          if !store.contains(index) && !waiting.contains(hash) => hash
    }.toSet

  /** The values of `hashes` that `reader` may read. */
  def readableBy(graph: AuthzEventGraph, reader: PublicIdentity, hashes: Iterable[Hash]): List[DeltaValue] =
    hashes.iterator.flatMap { hash =>
      graph.events.get(hash).flatMap((_, index) => store.get(index)).collect {
        case stored if Authorization.mayReadAssumingCommitmentHolds(reader, hash, stored.delta, graph) =>
          DeltaValue(hash, RevealedValue(writeToArray(stored.delta), stored.witness))
      }
    }.toList

  /** Remember the current state, see [[snapshot]]. */
  def createSnapshot(graph: AuthzEventGraph): AuthzDeltas[RDT] =
    copy(snapshot = Some(AuthzDeltas.Snapshot(graph.nextEventIndex - 1, state)))

  /** Stores the (accepted) delta of the known event `eventHash` and merges it into the state. */
  def put(graph: AuthzEventGraph, eventHash: Hash, delta: RDT, witness: Array[Byte]): AuthzDeltas[RDT] = {
    val index = graph.events(eventHash)._2
    copy(
      store = store.put(index, delta, witness),
      state = state.merge(delta),
      snapshot = snapshot.map(s => if index <= s.version then s.copy(state = s.state.merge(delta)) else s),
    )
  }

  /** Accepts a delta value of a known event, if its commitment holds, `self` may read it, and it was allowed to be
    * written.
    * @return whether the value was added
    * @throws IllegalArgumentException otherwise
    */
  def receiveChecked(
      graph: AuthzEventGraph,
      self: PublicIdentity,
      eventHash: Hash,
      value: RevealedValue
  ): (AuthzDeltas[RDT], Boolean) = {
    val (event, index) = graph.events(eventHash)
    if store.contains(index) then (this, false)
    else
        val commitment = value.commitment(event.author.id)
        require(event.payload == DeltaCommitment(commitment), "commitment does not hold")
        val delta = readFromArray[RDT](value.value)
        require(Authorization.mayReadAssumingCommitmentHolds(self, eventHash, delta, graph), "may not read")
        require(Authorization.mayWriteAssumingCommitmentHolds(graph, eventHash, event, delta), "may not write")
        (put(graph, eventHash, delta, value.witness), true)
  }

  /** Accepts a delta value from the network: buffered if its event is unknown, dropped if it is not acceptable.
    * @return whether the value was added
    */
  def receive(
      graph: AuthzEventGraph,
      self: PublicIdentity,
      eventHash: Hash,
      value: RevealedValue
  ): (AuthzDeltas[RDT], Boolean) =
    if !graph.events.contains(eventHash) then (copy(waiting = waiting.updated(eventHash, value)), false)
    else Try(receiveChecked(graph, self, eventHash, value)).getOrElse((this, false))

  /** Accepts the value of the newly added event `eventHash`, if it arrived before the event. */
  def receiveWaiting(graph: AuthzEventGraph, self: PublicIdentity, eventHash: Hash): (AuthzDeltas[RDT], Boolean) =
    waiting.get(eventHash) match
        case Some(value) => copy(waiting = waiting - eventHash).receive(graph, self, eventHash, value)
        case None        => (this, false)

  /** Removes the deltas the revocation invalidates and rematerializes, keeping the snapshot if it is unaffected.
    * @return whether any delta was removed
    */
  def invalidate(graph: AuthzEventGraph, revocation: Hash): (AuthzDeltas[RDT], Boolean) = {
    val invalidated = graph.deltasInvalidatedBy(revocation).filter(store.contains).toSet
    if invalidated.isEmpty then (this, false)
    else
        val keptSnapshot = snapshot.filterNot(s => invalidated.exists(_ <= s.version))
        val next         = copy(store = store.removedAll(invalidated), snapshot = keptSnapshot)
        (next.copy(state = next.rematerialized(graph)), true)
  }

  /** The state from the snapshot (if any) and the deltas of events after it. */
  private def rematerialized(graph: AuthzEventGraph): RDT = snapshot match
      case None           => store.merged
      case Some(snapshot) =>
        // walk back from the heads, through the events after the snapshot
        val toVisit = scala.collection.mutable.Queue.from(graph.heads)
        val visited = scala.collection.mutable.Set.from(graph.heads)
        var result  = snapshot.state
        while toVisit.nonEmpty do {
          val (event, index) = graph.events(toVisit.dequeue())
          if index > snapshot.version then
              store.get(index).foreach(stored => result = result.merge(stored.delta))
              val parents = event.parents.diff(visited)
              toVisit.enqueueAll(parents)
              visited.addAll(parents)
        }
        result
}
