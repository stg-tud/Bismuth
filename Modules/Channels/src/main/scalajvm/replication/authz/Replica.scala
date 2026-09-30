package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.JsoniterCodecsJvm.ardtEventCodec
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}

import scala.annotation.tailrec
import scala.collection.mutable

class Replica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy,
    onStateChange: RDT => Unit
) {
  val localReplicaId: PublicIdentity = privateIdentity.getPublic

  @volatile protected var eventGraph: ArdtEventGraph[RDT]       = ArdtEventGraph(genesis)
  @volatile protected var deltaValueStore: DeltaValueStore[RDT] = DeltaValueStore[RDT]()
  private lazy val antiEntropy: AntiEntropy                     = antiEntropyProvider(this)

  @volatile protected var materializedState: RDT = Bottom[RDT].empty

  def state: RDT                                                  = synchronized { materializedState }
  def heads: Set[Hash]                                            = eventGraph.heads
  def event(hash: Hash): Option[ArdtEvent]                        = eventGraph.events.get(hash).map(_._1)
  def allEventsInCausalOrder: Array[(Hash, ArdtEvent)]            = eventGraph.allEventsInCausalOrder
  def revealedDeltaValue(commitment: Hash): Option[RevealedValue] = deltaValueStore.getRevealedValue(commitment)
  def delta(commitment: Hash): Option[RDT]                        = deltaValueStore.get(commitment).map(_.delta)

  def listenAddress: Option[(String, Int)]  = antiEntropy.listenAddress
  def connect(address: (String, Int)): Unit = antiEntropy.connect(address)

  def start(): Unit = antiEntropy.start()
  def stop(): Unit  = antiEntropy.stop()

  def containsEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)

  /** Adds an event to the event graph and updates the materialized state accordingly.
    *
    * @return Right(Some(hash)) if the event was added, Right(None) if the event was already known, and
    *         Left(missing) if the event depends on events that are missing locally.
    */
  def receiveEvent(encodedEvent: Array[Byte]): Either[Set[Hash], Option[Hash]] = synchronized {
    val oldEventGraph = eventGraph
    oldEventGraph.receive(encodedEvent) match {
      case Right(updatedEventGraph) =>
        eventGraph = updatedEventGraph
        val addedEventHash = eventGraph.heads.diff(oldEventGraph.heads).headOption
        addedEventHash.foreach { eventHash =>
          val (event, _) = eventGraph.events(eventHash)
          event.payload match {
            case Revocation(_) =>
              if eventGraph.heads.size > 1 // Check if we have events that are concurrent to revocation.
              then
                  if invalidateDeltasAfterRevocation(eventHash) then {
                    rematerialize()
                    onStateChange(materializedState)
                  }
            case _ => // delta values are not accepted before their commitment, new delegations don't update the state
          }
        }
        Right(addedEventHash)
      case Left(missing) => Left(missing)
    }
  }

  // Deletes all deltas that depend on revoked capability that are not causallyBefore revocationEvent
  protected def invalidateDeltasAfterRevocation(revocationEventHash: Hash): Boolean = synchronized {
    removeDeltas(deltasInvalidatedBy(revocationEventHash))
  }

  /** The delta events that `revocationEventHash` newly invalidates: those whose value is still stored, that are
    * authorized by a capability it revokes (directly or transitively), and that are not causally before it.
    */
  protected def deltasInvalidatedBy(revocationEventHash: Hash): Iterable[(commitment: Hash, index: Int)] = synchronized {
    val evGraph             = eventGraph
    val revokedCapabilities =
      evGraph.revocationCache.filter((_, revocations) => revocations.contains(revocationEventHash)).keySet
    if revokedCapabilities.isEmpty then return Iterable.empty

    // Every use of a capability, like every capability delegated from it, is received after that capability, and thus
    // has a higher index. Every event up to the latest cut in the revocation's causal past is causally before it. A
    // single search of the revocation's causal past, cut off at the later of the two, therefore suffices to tell the
    // valid uses apart from the invalidated ones.
    val revocationParents = evGraph.events(revocationEventHash)._1.parents
    val cutoff            = math.max(
      revokedCapabilities.iterator.map(evGraph.events(_)._2).min,
      evGraph.latestCutBefore(revocationParents)
    )

    val causalPast = mutable.BitSet()
    val toVisit    = mutable.Stack.from(revocationParents)
    while toVisit.nonEmpty do {
      val (event, index) = evGraph.events(toVisit.pop())
      if index > cutoff && !causalPast.contains(index) then
          causalPast += index
          toVisit.pushAll(event.parents)
    }

    evGraph.events.collect {
      case (eventHash, (ArdtEvent(DeltaCommitment(commitment), _, _, _, authorization), index))
          if index > cutoff && revokedCapabilities.contains(authorization) && !causalPast.contains(index)
          && deltaValueStore.get(commitment).nonEmpty =>
        (commitment = commitment, index = index)
    }
  }

  /** Removes the delta values of `deltaEvents`, returning whether any of them was stored */
  protected def removeDeltas(deltaEvents: Iterable[(commitment: Hash, index: Int)]): Boolean = synchronized {
    var hasRemovedADelta = false
    deltaEvents.foreach(deltaEvent => hasRemovedADelta |= deltaValueStore.remove(deltaEvent.commitment).nonEmpty)
    hasRemovedADelta
  }

  protected def rematerialize(): Unit = synchronized {
    materializedState = deltaValueStore.merged
  }

  /** Stores a received delta value and merges it into the materialized state if it is authorized.
    *
    * @throws IllegalArgumentException if the local replica may not read the delta.
    */
  def receiveDelta(eventHash: Hash, deltaValue: RevealedValue): Unit = synchronized {
    val (event, eventIndex) = eventGraph.events(eventHash)
    val commitment          = deltaValue.commitment(event.author.id)
    require(commitment == event.payload.asInstanceOf[DeltaCommitment].commitment)

    val delta = readFromArray[RDT](deltaValue.value)
    require(Authorization.mayReadAssumingCommitmentHolds(localReplicaId, eventHash, delta, eventGraph))
    require(Authorization.mayWriteAssumingCommitmentHolds(eventGraph, eventHash, event, delta))

    deltaValueStore.put(commitment, delta, deltaValue.witness)
    applyDelta(delta, eventIndex)
    onStateChange(state)
  }

  protected def applyDelta(delta: RDT, eventIndex: Int): Unit = synchronized {
    materializedState = materializedState.merge(delta)
  }

  def mutateState(mutator: RDT => RDT): Unit = synchronized {
    val delta = mutator(state)
    eventGraph.capabilities(localReplicaId).find {
      case (hash, capability) =>
        Filter[RDT].isAllowed(delta, capability.write) && eventGraph.revocations(hash).isEmpty
    } match {
      case Some(hash, capability) => mutateState(delta, hash)
      case None => throw new IllegalArgumentException(s"Insufficient permissions for mutation: $delta")
    }
  }

  def mutateState(mutator: RDT => RDT, capability: Hash): Unit = synchronized {
    val delta = mutator(state)
    mutateState(delta, capability)
  }

  private def mutateState(delta: RDT, capabilityHash: Hash): Unit = synchronized {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    require(eventGraph.events(capabilityHash) match {
      case (ArdtEvent(Capability(`localReplicaId`, _, write), _, _, _, _), _) =>
        Filter[RDT].isAllowed(delta, write)
      case _ => false
    })

    val eventsWithDeltas: Iterable[(Hash, ArdtEvent, Array[Byte], RDT, RevealedValue)] =
        var parents = heads
        Decompose.decompose(delta).map { decomposedDelta =>
          val commitedValue      = Commitment.commit(localReplicaId.id, writeToArray(decomposedDelta))
          val payload            = DeltaCommitment(commitedValue.commitment(localReplicaId.id))
          val signedEvent        = createSignedEvent(payload, capabilityHash, parents)
          val encodedSignedEvent = writeToArray(signedEvent)
          val hash               = Hash.compute(encodedSignedEvent)
          parents = Set(hash)
          (hash, signedEvent, encodedSignedEvent, decomposedDelta, commitedValue)
        }

    // Apply locally, skipping redundant checks (e.g., signature)
    var evGraph      = eventGraph
    var updatedState = materializedState
    eventsWithDeltas.foreach((hash, event, _, delta, revealedValue) =>
        evGraph = evGraph.copy(
          heads = Set(hash),
          events = evGraph.events + (hash -> (event, evGraph.nextEventIndex)),
          latestCuts = evGraph.latestCuts :+ evGraph.nextEventIndex,
          nextEventIndex = evGraph.nextEventIndex + 1
        )
        deltaValueStore.put(event.payload.asInstanceOf[DeltaCommitment].commitment, delta, revealedValue.witness)
        updatedState = updatedState.merge(delta)
    )
    eventGraph = evGraph
    materializedState = updatedState

    disseminate(eventsWithDeltas)
  }

  protected def disseminate(eventsWithDeltas: Iterable[(Hash, ArdtEvent, Array[Byte], RDT, RevealedValue)]): Unit = {
    antiEntropy.broadcastEvents(eventsWithDeltas.map(_._3))
    antiEntropy.broadcastDeltasFiltered(eventsWithDeltas.map(d => d._1 -> d._5))
  }

  def createRevocation(revokedCapability: Hash): Unit = {
    @tailrec
    def findAuthorizationForRevocation(authEvent: Hash): Option[Hash] =
      if authEvent == Hash.allZeroHash then None
      else
          eventGraph.events(authEvent) match {
            case (ArdtEvent(_, author, _, _, parentAuthorization), _) =>
              if author == localReplicaId then Some(authEvent)
              else findAuthorizationForRevocation(parentAuthorization)
          }

    findAuthorizationForRevocation(revokedCapability) match {
      case Some(authorization) =>
        val revocationEvent =
          writeToArray(createSignedEvent(Revocation(revokedCapability), authorization, eventGraph.heads))
        // Apply event locally
        require(receiveEvent(revocationEvent).isRight)
        // Disseminate event
        antiEntropy.broadcastEvents(Iterable.single(revocationEvent))
      case None => throw new IllegalStateException("No capability in authorization chain found to perform revocation")
    }
  }

  def createDelegation(
      usedCapability: Hash,
      delegatee: PublicIdentity,
      readPermissions: PermissionTree,
      writePermissions: PermissionTree
  ): Unit = {
    require(writePermissions <= readPermissions)

    eventGraph.events(usedCapability) match {
      case (ArdtEvent(Capability(capabilityHolder, readUpperLimit, writeUpperLimit), _, _, _, _), _) =>
        require(capabilityHolder == localReplicaId)
        require(readPermissions <= readUpperLimit)
        require(writePermissions <= writeUpperLimit)
        // TODO: apply unchecked
        val delegationEvent = writeToArray(createSignedEvent(
          Capability(delegatee, readPermissions, writePermissions),
          usedCapability,
          eventGraph.heads
        ))

        // Apply event locally
        require(receiveEvent(delegationEvent).isRight)

        // Disseminate event
        antiEntropy.broadcastEvents(Iterable.single(delegationEvent))
      case _ => throw new IllegalArgumentException("Referenced capability event is not a capability")
    }
  }

  def activeCapabilities: Map[PublicIdentity, Set[(Hash, Capability)]] = eventGraph.activeCapabilities

  def activeCapabilitiesOf(publicIdentity: PublicIdentity): Set[(Hash, Capability)] =
    eventGraph.activeCapabilitiesOf(publicIdentity)

  private def createSignedEvent(payload: ArdtEvent.Payload, capability: Hash, parents: Set[Hash]): ArdtEvent = {
    val unsignedEvent = ArdtEvent(
      payload,
      localReplicaId,
      parents,
      Signature.allZeroSignature,
      capability
    )
    val signature = Signature.compute(writeToArray(unsignedEvent), privateIdentity.identityKey.getPrivate)
    unsignedEvent.copy(signature = signature)
  }

  def filterDeltas(
      readingReplica: PublicIdentity,
      deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]
  ): Iterable[(eventHash: Hash, delta: RevealedValue)] = synchronized {
    deltas.filter { case (eventHash, RevealedValue(encodedDelta, _)) =>
      Authorization.mayRead(readingReplica, eventHash, eventGraph, deltaValueStore)
    }
  }
}
