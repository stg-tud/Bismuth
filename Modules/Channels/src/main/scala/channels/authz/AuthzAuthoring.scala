package channels.authz

import AuthzEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Commitment, Hash, PrivateIdentity, PublicIdentity, Signature}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.base.Decompose
import rdts.filters.{Filter, PermissionTree}

import scala.annotation.tailrec

/** Creates the signed events of local operations on top of an [[AuthzEventGraph]], as `identity`.
  * Throws if `identity` is not permitted to perform the operation.
  *
  * Only creates events, adding them is up to the caller, e.g., [[AuthzReplica]].
  */
object AuthzAuthoring {

  /** An active capability of `identity` that allows writing `delta`. */
  def writeCapability[RDT: Filter](graph: AuthzEventGraph, identity: PublicIdentity, delta: RDT): Hash =
    graph.capabilities(identity).find { (hash, capability) =>
      Filter[RDT].isAllowed(delta, capability.write) && graph.revocations(hash).isEmpty
    } match
        case Some((hash, _)) => hash
        case None            => throw IllegalArgumentException(s"Insufficient permissions for mutation: $delta")

  /** Write `delta` using the capability `capabilityHash`. Decomposed deltas become a chain of events, each with the
    * value its commitment reveals.
    */
  def write[RDT: {JsonValueCodec, Filter, Decompose}](
      graph: AuthzEventGraph,
      identity: PrivateIdentity,
      delta: RDT,
      capabilityHash: Hash
  ): List[(event: AuthzEvent, value: RevealedValue)] = {
    require(graph.revocations(capabilityHash).isEmpty)
    require(graph.events.get(capabilityHash) match
        case Some((AuthzEvent(Capability(holder, _, write), _, _, _, _), _)) =>
          holder == identity.getPublic && Filter[RDT].isAllowed(delta, write)
        case _ => false)

    val (_, created) =
      Decompose.decompose(delta).foldLeft((graph.heads, List.empty[(event: AuthzEvent, value: RevealedValue)])) {
        case ((parents, created), part) =>
          val revealed = Commitment.commit(identity.getPublic.id, writeToArray(part))
          val event    =
            signed(identity, DeltaCommitment(revealed.commitment(identity.getPublic.id)), capabilityHash, parents)
          (Set(event.hash), (event, revealed) :: created)
      }
    created.reverse
  }

  /** Delegate a capability derived from `usedCapability` (held by `identity`) to `delegatee`. */
  def delegate(
      graph: AuthzEventGraph,
      identity: PrivateIdentity,
      usedCapability: Hash,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): AuthzEvent = {
    require(write <= read)
    graph.events.get(usedCapability) match
        case Some((AuthzEvent(Capability(holder, readLimit, writeLimit), _, _, _, _), _)) =>
          require(holder == identity.getPublic)
          require(read <= readLimit && write <= writeLimit)
          signed(identity, Capability(delegatee, read, write), usedCapability, graph.heads)
        case _ => throw IllegalArgumentException("Referenced capability event is not a capability")
  }

  /** Revoke `revokedCapability`, authorized by the closest capability held by `identity` in its authorization chain. */
  def revoke(graph: AuthzEventGraph, identity: PrivateIdentity, revokedCapability: Hash): AuthzEvent = {
    @tailrec
    def authorizationFor(authEvent: Hash): Option[Hash] =
      if authEvent == Hash.allZeroHash then None
      else
          graph.events(authEvent)._1 match
              case AuthzEvent(Capability(holder, _, _), _, _, _, _) if holder == identity.getPublic => Some(authEvent)
              case AuthzEvent(_, _, _, _, parent) => authorizationFor(parent)

    authorizationFor(revokedCapability) match
        case Some(authorization) => signed(identity, Revocation(revokedCapability), authorization, graph.heads)
        case None                =>
          throw IllegalStateException("No capability in authorization chain found to perform revocation")
  }

  private def signed(
      identity: PrivateIdentity,
      payload: AuthzEvent.Payload,
      capability: Hash,
      parents: Set[Hash]
  ): AuthzEvent = {
    val unsigned = AuthzEvent(payload, identity.getPublic, parents, Signature.allZeroSignature, capability)
    unsigned.copy(signature = identity.sign(writeToArray(unsigned)))
  }
}
