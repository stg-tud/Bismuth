package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

class Replica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: Hash,
    privateIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy
) {
  val localReplicaId: PublicIdentity = privateIdentity.getPublic

  def state: RDT                                    = Authorization.materialize(eventGraph, deltaValueStore)
  private var eventGraph: ArdtEventGraph[RDT]       = ArdtEventGraph(genesis)
  private val deltaValueStore: DeltaValueStore[RDT] = DeltaValueStore[RDT]()
  private lazy val antiEntropy: AntiEntropy         = antiEntropyProvider(this)

  def listenAddress: Option[(String, Int)] = antiEntropy.listenAddress

  def containsEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)

  def revokedCapabilities: Set[Hash] = ???

  def capabilities: Map[PublicIdentity, Set[(Hash, Capability)]] = ???

  def receiveEvent(encodedEvent: Array[Byte]): Either[Set[Hash], Hash] = {
    val oldHeads = eventGraph.heads
    eventGraph.receive(encodedEvent) match {
      case Right(updatedEventGraph) =>
        eventGraph = updatedEventGraph
        val addedEventHash = eventGraph.heads.diff(oldHeads).head
        Right(addedEventHash)
      case Left(missing) =>
        Left(missing)
    }
  }

  def receiveDelta(eventHash: Hash, delta: RevealedValue): Unit =
      require(Authorization.mayRead(localReplicaId, eventHash, eventGraph, deltaValueStore))
      deltaValueStore.put(delta)

  def mutateState(mutator: RDT => RDT): Unit = {
    val delta   = mutator(state)
    val revoked = revokedCapabilities
    capabilities(localReplicaId).find { case (hash, capability) =>
      Filter[RDT].isAllowed(delta, capability.write) && revoked.contains(hash)
    } match {
      case Some(hash, capability) => createUpdate(delta, hash)
      case None                   => ???
    }
  }

  def mutateState(mutator: RDT => RDT, capability: Hash): Unit =
      val delta = mutator(state)
      createUpdate(delta, capability)

  private def createUpdate(delta: RDT, capabilityHash: Hash): Unit = {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    require(eventGraph.events(capabilityHash) match {
      case (ArdtEvent(Capability(`localReplicaId`, _, write), _, _, _, _), _) =>
        Filter[RDT].isAllowed(delta, write)
      case _ => false
    })

    val eventsWithDeltas = Decompose.decompose(delta).map { decomposedDelta =>
      val commitedValue = Commitment.commit(writeToArray(decomposedDelta))
      val unsignedEvent = ArdtEvent(
        DeltaCommitment(commitedValue.commitment),
        localReplicaId,
        eventGraph.heads,
        null.asInstanceOf[Signature],
        capabilityHash
      )
      val signature = Signature.compute(writeToArray(unsignedEvent), privateIdentity.identityKey.getPrivate)

      val signedEvent = writeToArray(unsignedEvent.copy(signature = signature))
      (Hash.compute(signedEvent), signedEvent, commitedValue)
    }

    // Apply locally
    eventsWithDeltas.foreach((hash, event, delta) =>
        require(receiveEvent(event).isRight)
        receiveDelta(hash, delta)
    )

    // Disseminate updates
    antiEntropy.broadcastEvents(eventsWithDeltas.map(_._2))
    antiEntropy.broadcastDeltasFiltered(eventsWithDeltas.map(d => d._1 -> d._3))
  }

  def createRevocation: Unit = ???

  def createDelegation: Unit = ???

  def filterDeltas(
      readingReplica: PublicIdentity,
      deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]
  ): Iterable[(eventHash: Hash, delta: RevealedValue)] =
    deltas.filter { case (eventHash, RevealedValue(encodedDelta, _)) =>
      Authorization.mayRead(readingReplica, eventHash, eventGraph, deltaValueStore)
    }
}
