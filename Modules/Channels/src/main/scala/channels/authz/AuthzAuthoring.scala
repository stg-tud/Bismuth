package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Commitment, Hash, PrivateIdentity, PublicIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.base.Decompose
import rdts.filters.Permits

/** Creates the signed delta events of local writes on top of an [[AuthzEventGraph]], as `identity`.
  * Throws if `identity` is not permitted to perform the operation.
  *
  * Only creates events, adding them is up to the caller, e.g., [[AuthzReplica]].
  */
object AuthzAuthoring {

  /** An active capability of `identity` that allows writing `delta`. */
  def writeCapability[RDT: Permits](graph: AuthzEventGraph, identity: PublicIdentity, delta: RDT): Hash =
    graph.policy.writeTrees(identity).find((_, write) => Permits[RDT].isAllowed(delta, write)) match
        case Some((hash, _)) => hash
        case None            => throw IllegalArgumentException(s"Insufficient permissions for mutation: $delta")

  /** Write `delta` using the capability `capabilityHash`. Decomposed deltas become a chain of events, each with the
    * value its commitment reveals.
    */
  def write[RDT: {JsonValueCodec, Permits, Decompose}](
      graph: AuthzEventGraph,
      identity: PrivateIdentity,
      delta: RDT,
      capabilityHash: Hash
  ): List[(event: AuthzEvent, value: RevealedValue)] = {
    require(graph.revocations(capabilityHash).isEmpty)
    require(graph.events.get(capabilityHash) match
        case Some((AuthzEvent(Capability(holder, _, write), _, _, _, _), _)) =>
          holder == identity.getPublic && Permits[RDT].isAllowed(delta, write)
        case _ => false)

    val (_, created) =
      Decompose.decompose(delta).foldLeft((graph.heads, List.empty[(event: AuthzEvent, value: RevealedValue)])) {
        case ((parents, created), part) =>
          val revealed = Commitment.commit(identity.getPublic.id, writeToArray(part))
          val event    =
            AuthzEvent.signed(identity, DeltaCommitment(revealed.commitment(identity.getPublic.id)), capabilityHash, parents)
          (Set(event.hash), (event, revealed) :: created)
      }
    created.reverse
  }

}
