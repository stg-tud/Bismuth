package ex2026accessControl.authz.evaluation

import channels.authz.AuthzCodecs.ardtEventCodec
import channels.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.{Commitment, Hash, PrivateIdentity, Signature}
import channels.crypto.Ed25519.given
import channels.authz
import channels.authz.{ArdtEvent, ArdtEventGraph, AuthzReplica}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.filters.PermissionTree

/** Helpers for directly constructing and inserting signed [[authz.ArdtEvent]]s, without going through
  * [[AuthzReplica]].
  * Used to assemble [[authz.ArdtEventGraph]]s for evaluation purposes.
  */
object EventGraphBuilder {

  def buildEvent(
      payload: ArdtEvent.Payload,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent = {
    val unsigned  = ArdtEvent(payload, author.getPublic, parents, Signature.allZeroSignature, authorization)
    val signature = author.sign(writeToArray(unsigned))
    unsigned.copy(signature = signature)
  }

  def buildCapabilityEvent(
      holder: channels.crypto.PublicIdentity,
      read: PermissionTree,
      write: PermissionTree,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent =
    buildEvent(Capability(holder, read, write), author, parents, authorization)

  def buildRevocationEvent(
      revokedCapability: Hash,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent =
    buildEvent(Revocation(revokedCapability), author, parents, authorization)

  def buildDeltaEvent[T: JsonValueCodec](
      delta: T,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): (ArdtEvent, RevealedValue) = {
    val revealed = Commitment.commit(author.getPublic.id, writeToArray(delta))
    val event    = buildEvent(DeltaCommitment(revealed.commitment(author.getPublic.id)), author, parents, authorization)
    (event, revealed)
  }

  /** Inserts an event into the graph, throwing if it is invalid or references unknown dependencies. */
  def receiveOrThrow[T](graph: ArdtEventGraph[T], event: ArdtEvent): ArdtEventGraph[T] =
    graph.receive(writeToArray(event)) match {
      case Right(updated) => updated
      case Left(missing)  =>
        throw new IllegalStateException(s"Event ${event.hash} is missing dependencies: $missing")
    }
}
