package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

class Replica[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    genesis: ArdtEvent,
    localIdentity: PrivateIdentity,
    antiEntropyProvider: Replica[?] => AntiEntropy
) {
  val localReplicaId: PublicIdentity   = localIdentity.getPublic
  private val antiEntropy: AntiEntropy = antiEntropyProvider(this)

  def state: RDT                                    = Authorization.materialize(eventGraph, deltaValueStore)
  private var eventGraph: ArdtEventGraph[RDT]       = ArdtEventGraph(genesis)
  private val deltaValueStore: DeltaValueStore[RDT] = DeltaValueStore[RDT]()

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

  def createUpdate(mutator: RDT => RDT, capabilityHash: Hash): Unit = {
    require(eventGraph.revocations(capabilityHash).isEmpty)
    val delta = mutator(state)
    require(eventGraph.events(capabilityHash) match {
      case (ArdtEvent(Capability(`localReplicaId`, _, write), _, _, _, _), _) =>
        Filter[RDT].isAllowed(delta, write)
      case _ => false
    })

    val eventsWithDeltas = Decompose.decompose(delta).map { decomposedDelta =>
      val commitedValue = Commitment.commit(writeToArray(decomposedDelta))
      val unsignedEvent =
        ArdtEvent(DeltaCommitment(commitedValue.commitment), localReplicaId, eventGraph.heads, null, capabilityHash)
      val signature = Signature.compute(writeToArray(unsignedEvent), localIdentity.identityKey.getPrivate)

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
      val delta = readFromArray[RDT](encodedDelta)
      Authorization.mayRead(readingReplica, eventHash, eventGraph, deltaValueStore)
    }

  def storesEvent(eventHash: Hash): Boolean = eventGraph.events.contains(eventHash)
}
