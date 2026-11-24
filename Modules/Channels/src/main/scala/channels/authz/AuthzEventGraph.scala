package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.{Hash, PublicIdentity, Verifier}
import rdts.base.Lattice
import rdts.filters.PermissionTree

import scala.collection.mutable

case class AuthzEventGraph(
    dag: AuthzDag,
    private[authz] val revocationCache: Map[Hash, Set[Hash]],
    private[authz] val capabilityCache: Map[PublicIdentity, Set[(Hash, Capability)]],
) {
  export dag.{
    genesis,
    heads,
    events,
    nextEventIndex,
    latestCutBefore,
    notBefore,
    causallyBefore,
    causallyAfter,
    concurrent,
    causalOrder,
    allEventsInCausalOrder
  }

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte])(using verifier: Verifier): Either[Set[Hash], AuthzEventGraph] = {
    // Check for duplicates before checking signature
    val eventHash = Hash.compute(encodedEvent)
    if events.contains(eventHash) then return Right(this)

    // Ensure that no invalid events are stored
    val event = dag.verified(eventHash, encodedEvent)
    if eventHash == genesis then require(event.payload.isInstanceOf[Capability])

    // Used capability is locally known (implies validity) and both holder and event author are the same
    val authorizingCapability: Capability = events.get(event.authorization) match {
      case None => // Return missing capability and heads
        val missingParents = event.parents.filterNot(events.contains)
        if eventHash != genesis then return Left(missingParents + event.authorization)
        else Capability(event.author, PermissionTree.allow, PermissionTree.allow)
      case Some((AuthzEvent(cap @ Capability(capabilityHolder, _, _), _, _, _, _), _)) =>
        // Used capability matches the event author
        require(capabilityHolder == event.author)
        cap
      case _ => // Referenced capability is not a capability event
        throw java.lang.IllegalArgumentException(s"Event with invalid capability: $event")
    }

    // All parents, and a revoked capability, are locally available
    val missingParents = event.parents.filterNot(events.contains) ++ (event.payload match
        case Revocation(revokedCapability) if !events.contains(revokedCapability) => Some(revokedCapability)
        case _                                                                    => None)
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
    Right(copy(
      dag = dag.added(eventHash, event),
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

  /** The indices of the delta events that `revocationEventHash` invalidates: uses of a revoked capability that are not
    * in the causal past of the revocation.
    */
  def deltasInvalidatedBy(revocationEventHash: Hash): Iterable[Int] = {
    val revokedCapabilities =
      revocationCache.filter((_, revocations) => revocations.contains(revocationEventHash)).keySet
    if revokedCapabilities.isEmpty then return Iterable.empty

    // Every use of a capability, like every capability delegated from it, is received after that capability, and thus
    // has a higher index. Every event up to the latest cut in the revocation's causal past is causally before it. A
    // single search of the revocation's causal past, cut off at the later of the two, therefore suffices to tell the
    // valid uses apart from the invalidated ones.
    val revocationParents = events(revocationEventHash)._1.parents
    val cutoff            = math.max(
      revokedCapabilities.iterator.map(events(_)._2).min,
      latestCutBefore(revocationParents)
    )

    val causalPast = mutable.BitSet()
    val toVisit    = mutable.Stack.from(revocationParents)
    while toVisit.nonEmpty do {
      val (event, index) = events(toVisit.pop())
      if index > cutoff && !causalPast.contains(index) then
          causalPast += index
          toVisit.pushAll(event.parents)
    }

    events.collect {
      case (_, (AuthzEvent(_: DeltaCommitment, _, _, _, authorization), index))
          if index > cutoff && revokedCapabilities.contains(authorization) && !causalPast.contains(index) =>
        index
    }
  }

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

}

object AuthzEventGraph {
  def apply(genesis: AuthzEvent): AuthzEventGraph =
    genesis.payload match {
      case cap @ Capability(holder, _, _) =>
        AuthzEventGraph(AuthzDag(genesis), Map.empty, Map(holder -> Set((genesis.hash, cap))))
      case _ => ???
    }

  def apply(genesis: Hash): AuthzEventGraph = AuthzEventGraph(AuthzDag(genesis), Map.empty, Map.empty)
}
