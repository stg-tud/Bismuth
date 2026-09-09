package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}

import scala.annotation.tailrec

class Replica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy,
    onStateChange: RDT => Unit
) {
  val localReplicaId: PublicIdentity = privateIdentity.getPublic

  def state: RDT                                    = Authorization.materialize(eventGraph, deltaValueStore)
  private var eventGraph: ArdtEventGraph[RDT]       = ArdtEventGraph(genesis)
  private val deltaValueStore: DeltaValueStore[RDT] = DeltaValueStore[RDT]()
  private lazy val antiEntropy: AntiEntropy         = antiEntropyProvider(this)

  def listenAddress: Option[(String, Int)] = antiEntropy.listenAddress
  def connect(address: (String, Int)): Unit = antiEntropy.connect(address)

  def start(): Unit = antiEntropy.start()
  def stop(): Unit  = antiEntropy.stop()

  def containsEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)

  def receiveEvent(encodedEvent: Array[Byte]): Either[Set[Hash], Option[Hash]] = synchronized {
    val oldHeads = eventGraph.heads
    eventGraph.receive(encodedEvent) match {
      case Right(updatedEventGraph) =>
        eventGraph = updatedEventGraph
        val addedEventHash = eventGraph.heads.diff(oldHeads).headOption
        Right(addedEventHash)
      case Left(missing) =>
        Left(missing)
    }
  }

  def receiveDelta(eventHash: Hash, delta: RevealedValue): Unit = synchronized {
    require(Authorization.mayRead(localReplicaId, eventHash, delta, eventGraph))
    deltaValueStore.put(delta)
    onStateChange(state)
  }

  def mutateState(mutator: RDT => RDT): Unit = synchronized {
    val delta = mutator(state)
    eventGraph.capabilities(localReplicaId).find {
      case (hash, capability) =>
        Filter[RDT].isAllowed(delta, capability.write) && eventGraph.revocations(hash).isEmpty
    } match {
      case Some(hash, capability) => createUpdate(delta, hash)
      case None                   => ???
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
    def findAuthorizationForRevocation(event: Hash): Option[Hash] =
      if event == Hash.allZeroHash then None
      else
          eventGraph.events(event) match {
            case (ArdtEvent(_, author, _, _, authorization), _) =>
              if author == localReplicaId then Some(revokedCapability)
              else findAuthorizationForRevocation(authorization)
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
