package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.Lattice
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import replication.authz.CausalOrder.*

import scala.collection.mutable

case class ArdtEventGraph[T: Lattice](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, (ArdtEvent, Int)],
    private[authz] val revocationCache: Map[Hash, Set[Hash]],
    private[authz] val capabilityCache: Map[PublicIdentity, Set[(Hash, Capability)]],
    private[authz] val nextEventIndex: Int
) {

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], ArdtEventGraph[T]] = {
    // Check for duplicates before checking signature
    val hash = Hash.compute(encodedEvent)
    if events.contains(hash) then return Right(this)

    val event: ArdtEvent = readFromArray(encodedEvent)
    // Ensure that no invalid events are stored
    // Signature verification: (need to blank signature and re-encode for verification)
    require(event.signature.verify(
      event.author.publicKey,
      writeToArray(event.copy(signature = Signature.allZeroSignature))
    ))

    // All events need predecessors except the genesis event
    if hash != genesis then {
      require(event.parents.nonEmpty)
    } else {
      require(event.parents.isEmpty)
      require(event.authorization == Hash.allZeroHash)
      require(event.payload.isInstanceOf[Capability])
    }

    // Used capability is locally known (implies validity) and both holder and event author are the same
    val authorizingCapability: Capability = events.get(event.authorization) match {
      case None => // Return missing capability and heads
        val missingParents = event.parents.filter(events.contains)
        if hash != genesis then return Left(missingParents + event.authorization)
        else Capability(event.author, PermissionTree.allow, PermissionTree.allow)
      case Some((ArdtEvent(cap @ Capability(capabilityHolder, _, _), _, _, _, _), _)) =>
        // Used capability matches the event author
        require(capabilityHolder == event.author)
        cap
      case _ => // Referenced capability is not a capability event
        throw java.lang.IllegalArgumentException(s"Event with invalid capability: $event")
    }

    // All parents are locally available
    val missingParents = event.parents.filterNot(events.contains)
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
      },
      capabilityCache = event.payload match {
        case capability @ Capability(holder, _, _) =>
          capabilityCache.updatedWith(holder) {
            case Some(oldCache) => Some(oldCache + (hash -> capability))
            case None           => Some(Set(hash -> capability))
          }
        case _ => capabilityCache
      }
    ))
  }

  // This performs an optimized BFS for computing the reachability from event2 to event1 along the predecessors.
  def causallyBefore(event1: Hash, event2: Hash): Boolean = {
    if event1 == event2 then return false

    val (ev1, ev1Idx, ev2, ev2Idx) = (events.get(event1), events.get(event2)) match {
      case (Some((ev1, ev1Idx)), Some((ev2, ev2Idx))) => (ev1, ev1Idx, ev2, ev2Idx)
      case _                                          => return false
    }

    // If ev1Idx > ev2Idx, we applied ev1 after ev2, thus ev1 is not reachable by ev2
    if ev1Idx > ev2Idx then return false
    if ev2.parents.contains(event1) then return true

    val toSearch = mutable.Queue.from(ev2.parents)
    val searched = mutable.Set(event2)

    while toSearch.nonEmpty do {
      val next                = toSearch.dequeue()
      val (nextEv, nextEvIdx) = events(next)

      if nextEvIdx >= ev1Idx then // If nextEvIdx < ev1Idx, then ev1 is not reachable via nextEv
          val parents = nextEv.parents
          if parents.contains(event1) then return true
          toSearch.enqueueAll(nextEv.parents.diff(searched))

      searched += next
    }

    false
  }

  def causallyAfter(event1: Hash, event2: Hash): Boolean = causallyBefore(event2, event1)

  def concurrent(event1: Hash, event2: Hash): Boolean =
    events.contains(event1) && events.contains(event2) &&
    !causallyAfter(event1, event2) && !causallyAfter(event2, event1)

  def causalOrder(event1: Hash, event2: Hash): CausalOrder =
    if !events.contains(event1) || !events.contains(event2) then UNKNOWN
    else if event1 == event2 then EQUAL
    else if causallyBefore(event1, event2) then BEFORE
    else if causallyBefore(event2, event1) then AFTER
    else CONCURRENT

  def authorizationChain(capHash: Hash): Seq[Hash] =
    if capHash == genesis then Seq(genesis)
    else capHash +: authorizationChain(events(capHash)._1.authorization) // assumes that chain is in local event graph

  def revocations(capHash: Hash): Set[Hash] =
    if capHash == genesis then revocationCache.getOrElse(capHash, Set.empty)
    else revocationCache.getOrElse(capHash, Set.empty) ++ revocations(events(capHash)._1.authorization)

  def capabilities(replicaId: PublicIdentity): Set[(Hash, Capability)] =
    capabilityCache.getOrElse(replicaId, Set.empty)
}

object ArdtEventGraph {
  def apply[T: Lattice](genesis: ArdtEvent): ArdtEventGraph[T] = {
    val hash = genesis.hash
    genesis.payload match {
      case cap @ Capability(holder, _, _) =>
        ArdtEventGraph(hash, Set(hash), Map(hash -> (genesis, 0)), Map.empty, Map(holder -> Set((hash, cap))), 1)
      case _ => ???
    }
  }

  def apply[T: Lattice](genesis: Hash): ArdtEventGraph[T] =
    ArdtEventGraph(genesis, Set.empty, Map.empty, Map.empty, Map.empty, 0)
}

enum CausalOrder:
    case BEFORE
    case AFTER
    case CONCURRENT
    case EQUAL
    case UNKNOWN
