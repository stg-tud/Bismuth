package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import crypto.Commitment.RevealedValue
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Lattice}
import rdts.filters.Filter
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

object Authorization {
  def materialize[T: {Lattice, Bottom, JsonValueCodec, Filter}](
      eventGraph: EventGraph[T],
      deltaValueStore: DeltaValueStore[T]
  ): T = {
    eventGraph.events.iterator.foldLeft(Bottom[T].empty) {
      case (left, (deltaEventHash, (deltaEvent @ ArdtEvent(deltaCommitment: DeltaCommitment, _, _, _, _), _))) =>
        deltaValueStore.get(deltaCommitment.commitment)
          .map(commited => readFromArray[T](commited.value))
          .filter(rdt => mayWrite(eventGraph, deltaEventHash, deltaEvent, rdt))
          .map(left.merge)
          .getOrElse(left)
      case (left, _) => left
    }
  }

  def mayRead[T](replica: PublicIdentity, delta: Hash, eventGraph: EventGraph[T]): Option[Boolean] =
    ???

  def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: EventGraph[T],
      deltaEventHash: Hash,
      deltaValue: RevealedValue
  ): Boolean = {
    val (deltaEvent, _) = eventGraph.events(deltaEventHash)
    mayWrite(eventGraph, deltaEventHash, deltaEvent, deltaValue)
  }

  private def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: EventGraph[T],
      deltaEventHash: Hash,
      deltaEvent: ArdtEvent,
      revealedValue: RevealedValue
  ): Boolean = {
    deltaEvent.payload match {
      case DeltaCommitment(commitment) =>
        if revealedValue.commitment != commitment then return false
        val delta = readFromArray[T](revealedValue.value)
        mayWrite(eventGraph, deltaEventHash, deltaEvent, delta)
      case _ => throw IllegalArgumentException(s"$deltaEvent is not a delta commitment")
    }
  }

  private def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: EventGraph[T],
      deltaEventHash: Hash,
      deltaEvent: ArdtEvent,
      delta: T
  ): Boolean = {
    // Delegation & revocation validity and validity of capability use are invariants of eventgraph, thus not checked here
    eventGraph.events(deltaEvent.authorization)._1.payload match {
      case Capability(_, _, write) =>
        val revocations =
          eventGraph
            .revocations(deltaEvent.authorization)
            .filter(revocation => !eventGraph.causallyBefore(deltaEventHash, revocation))
        Filter[T].isAllowed(delta, write) && revocations.isEmpty
      case _ => false
    }
  }
}
