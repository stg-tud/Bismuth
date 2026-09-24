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
    val oldHeads = eventGraph.heads
    eventGraph.receive(encodedEvent) match {
      case Right(updatedEventGraph) =>
        eventGraph = updatedEventGraph
        val addedEventHash = eventGraph.heads.diff(oldHeads).headOption
        addedEventHash.foreach { eventHash =>
          val (event, _) = eventGraph.events(eventHash)
          event.payload match {
            case Revocation(_) =>
              if eventGraph.heads.size > 1 // Check if we have events that are concurrent to revocation.
              then
                  if invalidateDeltasAfterRevocation(eventHash) > 0 then
                      materializedState = deltaValueStore.merged
                      onStateChange(materializedState)
            case _ => // delta values are not accepted before their commitment, new delegations don't update the state
          }
        }
        Right(addedEventHash)
      case Left(missing) => Left(missing)
    }
  }

  // Deletes all deltas that depend on revoked capability that are not causallyBefore revocationEvent
  private def invalidateDeltasAfterRevocation(revocationEventHash: Hash): Int = synchronized {
    val transitivelyRevokedCapabilities = eventGraph.events(revocationEventHash) match {
      case (ArdtEvent(Revocation(revokedCapability), _, _, _, _), _) =>
        eventGraph.capabilityCache.values.flatMap(caps =>
          caps.filter((capEvHash, _) => eventGraph.authorizationChain(capEvHash).contains(revokedCapability)).map(_._1)
        ).toSet
      case _ => ???
    }

    require(eventGraph.heads.contains(revocationEventHash))
    eventGraph.events.count {
      case evHash -> (ArdtEvent(DeltaCommitment(commitmentHash), _, _, _, auth), idx) =>
        if transitivelyRevokedCapabilities.contains(auth) && !eventGraph.causallyBefore(evHash, revocationEventHash)
        then deltaValueStore.remove(commitmentHash).nonEmpty
        else false
      case _ => false
    }
  }

  /** Stores a received delta value and merges it into the materialized state if it is authorized.
    *
    * @throws IllegalArgumentException if the local replica may not read the delta.
    */
  def receiveDelta(eventHash: Hash, deltaValue: RevealedValue): Unit = synchronized {
    val event      = eventGraph.events(eventHash)._1
    val commitment = deltaValue.commitment
    require(commitment == event.payload.asInstanceOf[DeltaCommitment].commitment)

    val delta = readFromArray[RDT](deltaValue.value)
    require(Authorization.mayReadAssumingCommitmentHolds(localReplicaId, eventHash, delta, eventGraph))
    require(Authorization.mayWriteAssumingCommitmentHolds(eventGraph, eventHash, event, delta))

    deltaValueStore.put(commitment, delta, deltaValue.witness)
    materializedState = materializedState.merge(delta)
    onStateChange(state)
  }

  def mutateState(mutator: RDT => RDT): Unit = synchronized {
    val delta = mutator(state)
    eventGraph.capabilities(localReplicaId).find {
      case (hash, capability) =>
        Filter[RDT].isAllowed(delta, capability.write) && eventGraph.revocations(hash).isEmpty
    } match {
      case Some(hash, capability) => createUpdate(delta, hash)
      case None => throw new IllegalArgumentException(s"Insufficient permissions for mutation: $delta")
    }
  }

  def mutateState(mutator: RDT => RDT, capability: Hash): Unit = synchronized {
    val delta = mutator(state)
    createUpdate(delta, capability)
  }

  private def createUpdate(delta: RDT, capabilityHash: Hash): Unit = synchronized {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    require(eventGraph.events(capabilityHash) match {
      case (ArdtEvent(Capability(`localReplicaId`, _, write), _, _, _, _), _) =>
        Filter[RDT].isAllowed(delta, write)
      case _ => false
    })

    val eventsWithDeltas = Decompose.decompose(delta).map { decomposedDelta =>
      val commitedValue = Commitment.commit(writeToArray(decomposedDelta))
      val payload       = DeltaCommitment(commitedValue.commitment)
      val signedEvent   = createSignedEvent(payload, capabilityHash)
      (Hash.compute(signedEvent), signedEvent, commitedValue)
    }

    // Apply locally
    eventsWithDeltas.foreach((eventHash, event, delta) =>
        require(receiveEvent(event).isRight)
        receiveDelta(eventHash, delta)
    )

    // Disseminate updates
    antiEntropy.broadcastEvents(eventsWithDeltas.map(_._2))
    antiEntropy.broadcastDeltasFiltered(eventsWithDeltas.map(d => d._1 -> d._3))
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
        val revocationEvent = createSignedEvent(Revocation(revokedCapability), authorization)
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
        val delegationEvent = createSignedEvent(
          Capability(delegatee, readPermissions, writePermissions),
          usedCapability
        )

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

  private def createSignedEvent(payload: ArdtEvent.Payload, capability: Hash): Array[Byte] = {
    val unsignedEvent = ArdtEvent(
      payload,
      localReplicaId,
      eventGraph.heads,
      Signature.allZeroSignature,
      capability
    )
    val signature = Signature.compute(writeToArray(unsignedEvent), privateIdentity.identityKey.getPrivate)
    writeToArray(unsignedEvent.copy(signature = signature))
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
