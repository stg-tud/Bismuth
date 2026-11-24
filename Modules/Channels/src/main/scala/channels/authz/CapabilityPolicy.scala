package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.{Hash, PrivateIdentity, PublicIdentity}
import rdts.base.Lattice
import rdts.filters.PermissionTree

import scala.annotation.tailrec
import scala.collection.mutable

/** The capabilities and revocations of an [[AuthzDag]]: validates capability use, delegations, and revocations of new
  * events, and answers which capabilities exist and whether they are revoked.
  * Knows nothing about deltas, see [[Authorization]] for decisions about them.
  */
case class CapabilityPolicy(
    private[authz] val revocationCache: Map[Hash, Set[Hash]],
    private[authz] val capabilityCache: Map[PublicIdentity, Set[(Hash, Capability)]],
) {

  /** Checks that `event` may be added to `dag`.
    * @throws IllegalArgumentException If the event is invalid
    * @return Left(missing) if events the check depends on are missing from `dag`
    */
  def check(dag: AuthzDag, eventHash: Hash, event: AuthzEvent): Either[Set[Hash], Unit] = {
    val events = dag.events
    if eventHash == dag.genesis then require(event.payload.isInstanceOf[Capability])

    // Used capability is locally known (implies validity) and both holder and event author are the same
    val authorizingCapability: Capability = events.get(event.authorization) match {
      case None => // Return missing capability and heads
        val missingParents = event.parents.filterNot(events.contains)
        if eventHash != dag.genesis then return Left(missingParents + event.authorization)
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
        require(authorizationChain(dag, revokedCapability).contains(event.authorization))
    }
    Right(())
  }

  /** Includes a [[check]]ed event, `dag` is the DAG before adding it. */
  def added(dag: AuthzDag, eventHash: Hash, event: AuthzEvent): CapabilityPolicy =
    copy(
      revocationCache = event.payload match {
        case DeltaCommitment(_)            => revocationCache
        case Revocation(revokedCapability) =>
          val transitivelyRevoked = capabilityCache.values.flatMap(caps =>
            caps.filter((capEvHash, _) =>
              authorizationChain(dag, capEvHash).contains(revokedCapability)
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
    )

  /** The indices of the delta events that `revocationEventHash` invalidates: uses of a revoked capability that are not
    * in the causal past of the revocation.
    */
  def deltasInvalidatedBy(dag: AuthzDag, revocationEventHash: Hash): Iterable[Int] = {
    val events              = dag.events
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
      dag.latestCutBefore(revocationParents)
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

  /** The write permissions of the capability `deltaEvent` uses, unless a revocation of it is not causally after
    * `deltaEvent`. None if the authorization is not a capability.
    */
  def writeTree(dag: AuthzDag, deltaEventHash: Hash, deltaEvent: AuthzEvent): Option[PermissionTree] =
    // Delegation & revocation validity and validity of capability use are invariants of the graph, thus not checked here
    dag.events(deltaEvent.authorization)._1.payload match {
      case Capability(_, _, write)
          if revocations(deltaEvent.authorization).forall(revocation =>
            dag.causallyBefore(deltaEventHash, revocation)
          ) => Some(write)
      case _ => None
    }

  /** The read permissions of the capabilities of `reader` that are not revoked causally before `deltaEventHash`. */
  def readTrees(dag: AuthzDag, reader: PublicIdentity, deltaEventHash: Hash): Iterator[PermissionTree] =
    capabilities(reader).iterator.collect {
      case (capabilityEventHash, capability)
          if revocations(capabilityEventHash).forall(revocation =>
            !dag.causallyBefore(revocation, deltaEventHash)
          ) => capability.read
    }

  /** The write permissions of the unrevoked capabilities of `writer`, by capability event. */
  def writeTrees(writer: PublicIdentity): Iterator[(Hash, PermissionTree)] =
    capabilities(writer).iterator.collect {
      case (capabilityEventHash, capability) if revocations(capabilityEventHash).isEmpty =>
        capabilityEventHash -> capability.write
    }

  /** Delegate a capability derived from `usedCapability` (held by `identity`) to `delegatee`, on top of `dag`.
    * Only creates the event, adding it is up to the caller.
    */
  def delegate(
      dag: AuthzDag,
      identity: PrivateIdentity,
      usedCapability: Hash,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): AuthzEvent = {
    require(write <= read)
    dag.events.get(usedCapability) match
        case Some((AuthzEvent(Capability(holder, readLimit, writeLimit), _, _, _, _), _)) =>
          require(holder == identity.getPublic)
          require(read <= readLimit && write <= writeLimit)
          AuthzEvent.signed(identity, Capability(delegatee, read, write), usedCapability, dag.heads)
        case _ => throw IllegalArgumentException("Referenced capability event is not a capability")
  }

  /** Revoke `revokedCapability`, authorized by the closest capability held by `identity` in its authorization chain,
    * on top of `dag`. Only creates the event, adding it is up to the caller.
    */
  def revoke(dag: AuthzDag, identity: PrivateIdentity, revokedCapability: Hash): AuthzEvent = {
    @tailrec
    def authorizationFor(authEvent: Hash): Option[Hash] =
      if authEvent == Hash.allZeroHash then None
      else
          dag.events(authEvent)._1 match
              case AuthzEvent(Capability(holder, _, _), _, _, _, _) if holder == identity.getPublic => Some(authEvent)
              case AuthzEvent(_, _, _, _, parent) => authorizationFor(parent)

    authorizationFor(revokedCapability) match
        case Some(authorization) => AuthzEvent.signed(identity, Revocation(revokedCapability), authorization, dag.heads)
        case None                =>
          throw IllegalStateException("No capability in authorization chain found to perform revocation")
  }

  def authorizationChain(dag: AuthzDag, capHash: Hash): Seq[Hash] =
    if capHash == dag.genesis then Seq(dag.genesis)
    else capHash +: authorizationChain(dag, dag.events(capHash)._1.authorization) // assumes that chain is in local event graph

  // TODO: could also cache whole chain
  def revocations(capHash: Hash): Set[Hash] = revocationCache.getOrElse(capHash, Set.empty)

  def capabilities(replicaId: PublicIdentity): Set[(Hash, Capability)] =
    capabilityCache.getOrElse(replicaId, Set.empty)

  def activeCapabilities: Map[PublicIdentity, Set[(Hash, Capability)]] =
    capabilityCache.map((k, v) => k -> v.filter((hash, _) => revocations(hash).isEmpty))

  def activeCapabilitiesOf(publicIdentity: PublicIdentity): Set[(Hash, Capability)] =
    capabilityCache.getOrElse(publicIdentity, Set.empty).filter((hash, _) => revocations(hash).isEmpty)
}

object CapabilityPolicy {

  /** The policy of a DAG containing only `genesis`. */
  def apply(genesis: AuthzEvent): CapabilityPolicy =
    genesis.payload match {
      case cap @ Capability(holder, _, _) => CapabilityPolicy(Map.empty, Map(holder -> Set((genesis.hash, cap))))
      case _                              => ???
    }

  val empty: CapabilityPolicy = CapabilityPolicy(Map.empty, Map.empty)
}
