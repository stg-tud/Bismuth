package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import crypto.Commitment.RevealedValue
import crypto.{Hash, PublicIdentity}
import rdts.filters.Filter
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

object Authorization {
  def materialize[T](eventGraph: EventGraph[T], deltaValueStore: DeltaValueStore[T]): T = ???

  def mayRead[T](replica: PublicIdentity, delta: Hash, eventGraph: EventGraph[T]): Option[Boolean] = ???

  def mayWrite[T: {JsonValueCodec, Filter}](
      eventGraph: EventGraph[T],
      deltaEventHash: Hash,
      deltaValue: RevealedValue
  ): Boolean = {
    // Delegation validity and validity of capability use are invariants of eventgraph, thus not checked here
    val (deltaEvent, _) = eventGraph.events(deltaEventHash)
    (deltaEvent.payload, eventGraph.events(deltaEvent.authorization)._1.payload) match {
      case (DeltaCommitment(commitment), Capability(_, _, write)) =>
        val delta       = readFromArray[T](deltaValue.value)
        val revocations =
          eventGraph
            .revocations(deltaEvent.authorization)
            .filter(revocation => !eventGraph.causallyBefore(deltaEventHash, revocation))
        Filter[T].isAllowed(delta, write) && deltaValue.commitment == commitment && revocations.isEmpty
      case _ => false
    }
  }
}
