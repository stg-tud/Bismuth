package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity, Signature}
import rdts.base.{Bottom, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

object Authorization {
  def materialize[T: {Lattice, Bottom, JsonValueCodec, Filter}](
      eventGraph: ArdtEventGraph[T],
      deltaValueStore: DeltaValueStore[T]
  ): T =
    eventGraph.events.iterator.foldLeft(Bottom[T].empty) {
      case (left, (deltaEventHash, (deltaEvent @ ArdtEvent(deltaCommitment: DeltaCommitment, _, _, _, _), _))) =>
        deltaValueStore.get(deltaCommitment.commitment)
          .map(commited => readFromArray[T](commited.value))
          .filter(rdt => mayWrite(eventGraph, deltaEventHash, deltaEvent, rdt))
          .map(left.merge)
          .getOrElse(left)
      case (left, _) => left
    }

  def mayRead[T: {JsonValueCodec, Filter}](
      replicaId: PublicIdentity,
      deltaEventHash: Hash,
      eventGraph: ArdtEventGraph[T],
      deltaValueStore: DeltaValueStore[T]
  ): Boolean =
    eventGraph.events.get(deltaEventHash) match {
      case Some(ArdtEvent(DeltaCommitment(commitment), _, _, _, _), _) =>
        val delta = deltaValueStore.get(commitment).map(deltaBytes => readFromArray[T](deltaBytes.value)).get
        mayRead(replicaId, deltaEventHash, delta, eventGraph)
      case _ => false
    }

  // assumes that delta matches hash and that the corresponding event is in the graph
  private[authz] def mayRead[T: Filter](
      replicaId: PublicIdentity,
      deltaEventHash: Hash,
      delta: T,
      eventGraph: ArdtEventGraph[T]
  ): Boolean =
    eventGraph.capabilities(replicaId)
      .exists((capabilityEventHash, capability) =>
        Filter[T].isAllowed(delta, capability.read) &&
        eventGraph
          .revocations(capabilityEventHash)
          .forall(revocation => !eventGraph.causallyBefore(revocation, deltaEventHash))
      )

  def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: ArdtEventGraph[T],
      deltaEventHash: Hash,
      deltaValue: RevealedValue
  ): Boolean = {
    val (deltaEvent, _) = eventGraph.events(deltaEventHash)
    mayWrite(eventGraph, deltaEventHash, deltaEvent, deltaValue)
  }

  private def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: ArdtEventGraph[T],
      deltaEventHash: Hash,
      deltaEvent: ArdtEvent,
      revealedValue: RevealedValue
  ): Boolean =
    deltaEvent.payload match {
      case DeltaCommitment(commitment) =>
        if revealedValue.commitment != commitment then return false
        val delta = readFromArray[T](revealedValue.value)
        mayWrite(eventGraph, deltaEventHash, deltaEvent, delta)
      case _ => throw IllegalArgumentException(s"$deltaEvent is not a delta commitment")
    }

  private def mayWrite[T: {Filter}](
      eventGraph: ArdtEventGraph[T],
      deltaEventHash: Hash,
      deltaEvent: ArdtEvent,
      delta: T
  ): Boolean =
    // Delegation & revocation validity and validity of capability use are invariants of eventgraph, thus not checked here
    eventGraph.events(deltaEvent.authorization)._1.payload match {
      case Capability(_, _, write) =>
        Filter[T].isAllowed(delta, write)
        && eventGraph // All revocations for cap must be causally after deltaEvent
          .revocations(deltaEvent.authorization)
          .forall(revocation => eventGraph.causallyBefore(deltaEventHash, revocation))
      case _ => false
    }

  def createGenesis(rootIdentity: PrivateIdentity): ArdtEvent = {
    val genesisEvent = ArdtEvent(
      Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.allow),
      rootIdentity.getPublic,
      Set.empty,
      Signature.allZeroSignature,
      Hash.allZeroHash
    )
    val encoded   = writeToArray(genesisEvent)
    val signature = Signature.compute(encoded, rootIdentity.identityKey.getPrivate)
    genesisEvent.copy(signature = signature)
  }
}
