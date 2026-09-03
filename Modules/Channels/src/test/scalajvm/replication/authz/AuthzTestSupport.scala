package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.*
import crypto.Commitment.RevealedValue
import crypto.channels.{IdentityFactory, PrivateIdentity}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}

import java.security.{KeyPair, PrivateKey}

object AuthzTestSupport extends munit.Assertions {
  given JsonValueCodec[Set[Int]] = JsonCodecMaker.make
  given Filter[Set[Int]]         = Filter.terminalSetFilter[Int]

  def newIdentity(): (PublicIdentity, PrivateKey) = {
    val keyPair: KeyPair = Ed25519Util.generateNewKeyPair
    (PublicIdentity.fromPublicKey(keyPair.getPublic), keyPair.getPrivate)
  }

  def buildEvent(
      payload: ArdtEvent.Payload,
      author: PublicIdentity,
      authorPrivateKey: PrivateKey,
      parents: Set[Hash],
      authorization: Hash
  ): ArdtEvent = {
    val unsigned  = ArdtEvent(payload, author, parents, Signature.allZeroSignature, authorization)
    val signature = Signature.compute(writeToArray(unsigned), authorPrivateKey)
    unsigned.copy(signature = signature)
  }

  def buildGenesis(holder: PublicIdentity, holderKey: PrivateKey): ArdtEvent =
    buildEvent(
      Capability(holder, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set.empty,
      Hash.allZeroHash
    )

  def receiveOrFail(graph: ArdtEventGraph[Set[Int]], event: ArdtEvent): ArdtEventGraph[Set[Int]] =
    graph.receive(writeToArray(event)) match {
      case Right(updated) => updated
      case Left(missing)  => fail(s"expected ${event.hash} to be accepted, but graph reported missing: $missing")
    }

  def freshGraph(): (ArdtEventGraph[Set[Int]], PublicIdentity, PrivateKey, ArdtEvent) = {
    val (holder, holderKey) = newIdentity()
    val genesis             = buildGenesis(holder, holderKey)
    (ArdtEventGraph[Set[Int]](genesis), holder, holderKey, genesis)
  }

  def newPrivateIdentity(): PrivateIdentity = IdentityFactory.createNewIdentity

  def buildDeltaEvent(
      delta: Set[Int],
      author: PublicIdentity,
      authorPrivateKey: PrivateKey,
      parents: Set[Hash],
      authorization: Hash
  ): (ArdtEvent, RevealedValue) = {
    val revealed = Commitment.commit(writeToArray(delta))
    val event    = buildEvent(DeltaCommitment(revealed.commitment), author, authorPrivateKey, parents, authorization)
    (event, revealed)
  }

}
