package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.Lattice
import rdts.filters.PermissionTree
import replication.JsoniterCodecsJvm.ardtEventCodec
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import replication.authz.CausalOrder.*

import scala.collection.mutable

case class ArdtEventGraph[T: Lattice](
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, (ArdtEvent, Int)],
    private[authz] val revocationCache: Map[Hash, Set[Hash]],
    private[authz] val capabilityCache: Map[PublicIdentity, Set[(Hash, Capability)]],
    nextEventIndex: Int,
    // By event index: the index of the latest cut in the causal past of the event, the event itself included. A cut is
    // an event that was the only head right after it was received, and thus has every event received before it in its
    // causal past. Receiving further events never changes these, since they don't change any event's causal past.
    private[authz] val latestCuts: Vector[Int]
) {

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], ArdtEventGraph[T]] = {
    // Check for duplicates before checking signature
    val eventHash = Hash.compute(encodedEvent)
    if events.contains(eventHash) then return Right(this)

    val event: ArdtEvent = readFromArray(encodedEvent)
    // Ensure that no invalid events are stored
    // Signature verification: (need to blank signature and re-encode for verification)
    require(event.signature.verify(
      event.author.publicKey,
      writeToArray(event.copy(signature = Signature.allZeroSignature))
    ))

    // All events need predecessors except the genesis event
    if eventHash != genesis then {
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
        if eventHash != genesis then return Left(missingParents + event.authorization)
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

    // Event is valid, update graph and return
    val concurrentHeads = heads -- event.parents
    Right(copy(
      heads = concurrentHeads + eventHash,
      events = events + (eventHash -> (event, nextEventIndex)),
      nextEventIndex = nextEventIndex + 1,
      latestCuts = latestCuts :+ (
        if concurrentHeads.isEmpty then nextEventIndex
        else latestCutBefore(event.parents)
      ),
      revocationCache = event.payload match {
        case DeltaCommitment(_)            => revocationCache
        case Revocation(revokedCapability) =>
          val transitivelyRevoked = capabilityCache.values.flatMap(caps =>
            caps.filter((capEvHash, _) =>
              authorizationChain(capEvHash).contains(revokedCapability)
            ).map(_._1 -> Set(eventHash))
          ).toMap

          Lattice.mapLattice(using Lattice.setLattice).merge(revocationCache, transitivelyRevoked)
        case Capability(_, _, _) =>
          // Transfer revocations of capability used for delegation to created capability
          val revocationsOfParentInAuthChain = revocations(event.authorization)
          if revocationsOfParentInAuthChain.isEmpty
          then revocationCache
          else revocationCache.updated(eventHash, revocationsOfParentInAuthChain)
      },
      capabilityCache = event.payload match {
        case capability @ Capability(holder, _, _) =>
          capabilityCache.updatedWith(holder) {
            case Some(oldCache) => Some(oldCache + (eventHash -> capability))
            case None           => Some(Set(eventHash -> capability))
          }
        case _ => capabilityCache
      }
    ))
  }

  /** The index of the latest cut in the causal past of any of `parents`, or -1 if there is none: every event with an
    * index up to it is causally before an event built on top of `parents`.
    */
  def latestCutBefore(parents: Set[Hash]): Int =
    parents.iterator.map(parent => latestCuts(events(parent)._2)).maxOption.getOrElse(-1)

  /** Whether `event1` is causally before `event2`, searching the causal past of `event2` backwards.
    *
    * An event received after `event2` cannot be causally before it. If `event1` is at most as old as the latest cut in the
    * causal past of `event2`, or of any event on the way, it is before it as well. Only events received after `event1`
    * need to be searched.
    */
  def causallyBefore(event1: Hash, event2: Hash): Boolean = {
    if event1 == event2 then return false

    val (index1, ev2, index2) = (events.get(event1), events.get(event2)) match {
      case (Some((_, index1)), Some((ev2, index2))) => (index1, ev2, index2)
      case _                                        => return false
    }

    if index1 > index2 then return false
    if index1 <= latestCuts(index2) then return true

    val visited = mutable.BitSet(index2)
    val toVisit = mutable.Stack.from(ev2.parents)
    while toVisit.nonEmpty do {
      val next = toVisit.pop()
      if next == event1 then return true
      val (nextEv, nextIndex) = events(next)
      if nextIndex > index1 && !visited.contains(nextIndex) then {
        if index1 <= latestCuts(nextIndex) then return true
        visited += nextIndex
        toVisit.pushAll(nextEv.parents)
      }
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

  // TODO: could also cache whole chain
  def revocations(capHash: Hash): Set[Hash] =
    if capHash == genesis then revocationCache.getOrElse(capHash, Set.empty)
    else revocationCache.getOrElse(capHash, Set.empty)

  def capabilities(replicaId: PublicIdentity): Set[(Hash, Capability)] =
    capabilityCache.getOrElse(replicaId, Set.empty)

  def activeCapabilities: Map[PublicIdentity, Set[(Hash, Capability)]] =
    capabilityCache.map((k, v) => k -> v.filter((hash, _) => revocations(hash).isEmpty))

  def activeCapabilitiesOf(publicIdentity: PublicIdentity): Set[(Hash, Capability)] =
    capabilityCache.getOrElse(publicIdentity, Set.empty).filter((hash, _) => revocations(hash).isEmpty)

  def allEventsInCausalOrder: Array[(Hash, ArdtEvent)] = {
    val sortedEvents = Array.ofDim[(Hash, ArdtEvent)](events.size)
    events.foreach { case (hash, (event, i)) => sortedEvents(i) = hash -> event }
    sortedEvents
  }
}

object ArdtEventGraph {
  def apply[T: Lattice](genesis: ArdtEvent): ArdtEventGraph[T] = {
    val hash = genesis.hash
    genesis.payload match {
      case cap @ Capability(holder, _, _) =>
        ArdtEventGraph(
          hash,
          Set(hash),
          Map(hash -> (genesis, 0)),
          Map.empty,
          Map(holder -> Set((hash, cap))),
          1,
          Vector(0)
        )
      case _ => ???
    }
  }

  def apply[T: Lattice](genesis: Hash): ArdtEventGraph[T] =
    ArdtEventGraph(genesis, Set.empty, Map.empty, Map.empty, Map.empty, 0, Vector.empty)
}

enum CausalOrder:
    case BEFORE
    case AFTER
    case CONCURRENT
    case EQUAL
    case UNKNOWN
