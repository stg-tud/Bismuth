package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, Signature}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}
import replication.authz.{ArdtEvent, ArdtEventGraph}

/** Helpers for directly constructing and inserting signed [[ArdtEvent]]s, without going through
  * [[replication.authz.Replica]] (which requires an actual network layer to disseminate events).
  * Used to assemble [[ArdtEventGraph]]s for evaluation purposes.
  */
object EventGraphBuilder {

  def buildEvent(
      payload: ArdtEvent.Payload,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent = {
    val unsigned  = ArdtEvent(payload, author.getPublic, parents, Signature.allZeroSignature, authorization)
    val signature = Signature.compute(writeToArray(unsigned), author.identityKey.getPrivate)
    unsigned.copy(signature = signature)
  }

  def buildCapabilityEvent(
      holder: crypto.PublicIdentity,
      read: PermissionTree,
      write: PermissionTree,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent =
    buildEvent(Capability(holder, read, write), author, parents, authorization)

  def buildDeltaEvent[T: JsonValueCodec](
      delta: T,
      author: PrivateIdentity,
      parents: Set[Hash],
      authorization: Hash
  ): (ArdtEvent, RevealedValue) = {
    val revealed = Commitment.commit(writeToArray(delta))
    val event    = buildEvent(DeltaCommitment(revealed.commitment), author, parents, authorization)
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
