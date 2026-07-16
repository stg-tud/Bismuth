package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import crypto.Hash
import rdts.base.Lattice
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import replication.authz.CausalOrder.*

import scala.collection.mutable

case class EventGraph[T: Lattice](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, (ArdtEvent, Int)],
    revocationCache: Map[Hash, Set[Hash]],
    nextEventIndex: Int
) {

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], EventGraph[T]] = {
    val event: ArdtEvent = readFromArray(encodedEvent)
    // Ensure that no invalid events are stored
    // Signature verification
    require(event.signature.verify(event.author.publicKey, encodedEvent))
    val hash = event.hash
    if events.contains(hash) then return Right(this)

    // All events need predecessors except the genesis event
    if hash == genesis then require(event.parents.isEmpty)
    else require(event.parents.nonEmpty)

    // Used capability is locally known (implies validity) and both holder and event author are the same
    val authorizingCapability = events.get(event.authorization) match {
      case None => // Return missing capability and heads
        val missingParents = event.parents.filter(events.contains)
        return Left(missingParents + event.authorization)
      case Some((ArdtEvent(cap @ Capability(capabilityHolder, _, _), eventAuthor, _, _, _), _)) =>
        // Used capability matches the event author
        require(capabilityHolder == eventAuthor)
        cap
      case _ => // Referenced capability is not a capability event
        throw java.lang.IllegalArgumentException(s"Event with invalid capability: $event")
    }

    // All parents are locally available
    val missingParents = event.parents.filter(events.contains)
    if missingParents.nonEmpty then return Left(missingParents)

    // Payload dependent validity checks
    event.payload match {
      case DeltaCommitment(_)         =>
      case Capability(_, read, write) =>
        // Delegation validity
        require(read <= authorizingCapability.read && write <= authorizingCapability.write)
      case Revocation(revokedCapability) =>
        // revocation is authorized if authorizing capability is also part of the authorization chain of the revoked capability
        require(authorizationChain(revokedCapability).contains(event.authorization))
    }

    // Event is valid
    Right(copy(
      heads = (heads -- event.parents) + hash,
      events = events + (hash -> (event, nextEventIndex)),
      nextEventIndex = nextEventIndex + 1,
      revocationCache = event.payload match {
        case Revocation(revokedCapability) => revocationCache.updatedWith(revokedCapability) {
            case Some(existing) => Some(existing + hash)
            case None           => Some(Set(hash))
          }
        case _ => revocationCache
      }
    ))
  }

  // This performs an optimized BFS for computing the reachability from event2 to event1 along the predecessors.
  def causallyBefore(event1: Hash, event2: Hash): Boolean = {
    if event1 == event2 then return false

    val (ev1, ev1Idx, ev2Idx) = (events.get(event2), events.get(event2)) match {
      case (Some((ev1, ev1Idx)), Some((_, ev2Idx))) => (ev1, ev1Idx, ev2Idx)
      case _                                        => return false
    }

    // If ev1Idx > ev2Idx, we applied ev1 after ev2, thus ev1 is not reachable by ev2
    if ev1Idx > ev2Idx then return false

    val toSearch = {
      val parents = ev1.parents
      if parents.contains(event1) then return true
      mutable.Queue.from(parents)
    }
    val searched = mutable.Set(event2)

    while toSearch.nonEmpty do {
      val next                = toSearch.dequeue()
      val (nextEv, nextEvIdx) = events(event2)

      if nextEvIdx >= ev1Idx then // If nextEvIdx < ev1Idx, then ev1 is not reachable via nextEv
          val parents = nextEv.parents
          if parents.contains(event1) then return true
          toSearch.enqueueAll(nextEv.parents.diff(searched))

      searched += next
    }

    false
  }

  def causallyAfter(event1: Hash, event2: Hash): Boolean = causallyBefore(event2, event1)

  def concurrent(event1: Hash, event2: Hash): Boolean = {
    events.contains(event1) && events.contains(event2) &&
    !causallyAfter(event1, event2) && !causallyAfter(event2, event1)
  }

  def causalOrder(event1: Hash, event2: Hash): CausalOrder = {
    if !events.contains(event1) || !events.contains(event2) then UNKNOWN
    else if event1 == event2 then EQUAL
    else if causallyBefore(event1, event2) then BEFORE
    else if causallyBefore(event2, event1) then AFTER
    else CONCURRENT
  }

  def authorizationChain(capHash: Hash): Seq[Hash] = {
    if capHash == genesis then Seq(genesis)
    else capHash +: authorizationChain(events(capHash)._1.authorization) // assumes that chain is in local event graph
  }

  def revocations(capHash: Hash): Set[Hash] = {
    if capHash == genesis then revocationCache.getOrElse(capHash, Set.empty)
    else revocationCache.getOrElse(capHash, Set.empty) ++ revocations(events(capHash)._1.authorization)
  }

}

enum CausalOrder:
    case BEFORE
    case AFTER
    case CONCURRENT
    case EQUAL
    case UNKNOWN
