package channels.authz

import channels.authz.AuthzEvent.Payload.{Capability, DeltaCommitment}
import channels.crypto.Commitment.RevealedValue
import channels.crypto.Ed25519.fromPublicKey
import channels.crypto.{Ed25519PrivateIdentity, Ed25519Util, IdentityFactory, PublicIdentity, *}
import channels.crypto.Ed25519.given

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.filters.{Permits, PermissionTree}

import java.security.{KeyPair, PrivateKey}

object AuthzTestSupport extends munit.Assertions {
  given JsonValueCodec[Set[Int]] = JsonCodecMaker.make
  given Permits[Set[Int]]        = Permits.terminalSetPermits[Int]

  def newIdentity(): (PublicIdentity, PrivateKey) = {
    val keyPair: KeyPair = Ed25519Util.generateNewKeyPair
    (PublicIdentity.fromPublicKey(keyPair.getPublic), keyPair.getPrivate)
  }

  def buildEvent(
      payload: AuthzEvent.Payload,
      author: PublicIdentity,
      authorPrivateKey: PrivateKey,
      parents: Set[Hash],
      authorization: Hash
  ): AuthzEvent = {
    val unsigned  = AuthzEvent(payload, author, parents, Signature.allZeroSignature, authorization)
    val signature = Ed25519.sign(writeToArray(unsigned), authorPrivateKey)
    unsigned.copy(signature = signature)
  }

  def buildGenesis(holder: PublicIdentity, holderKey: PrivateKey): AuthzEvent =
    buildEvent(
      Capability(holder, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set.empty,
      Hash.allZeroHash
    )

  def receiveOrFail(graph: AuthzEventGraph, event: AuthzEvent): AuthzEventGraph =
    graph.receive(writeToArray(event)) match {
      case Right(updated) => updated
      case Left(missing)  => fail(s"expected ${event.hash} to be accepted, but graph reported missing: $missing")
    }

  def freshGraph(): (AuthzEventGraph, PublicIdentity, PrivateKey, AuthzEvent) = {
    val (holder, holderKey) = newIdentity()
    val genesis             = buildGenesis(holder, holderKey)
    (AuthzEventGraph(genesis), holder, holderKey, genesis)
  }

  def newPrivateIdentity(): Ed25519PrivateIdentity = IdentityFactory.createNewIdentity

  def buildDeltaEvent(
      delta: Set[Int],
      author: PublicIdentity,
      authorPrivateKey: PrivateKey,
      parents: Set[Hash],
      authorization: Hash
  ): (AuthzEvent, RevealedValue) = {
    val revealed = Commitment.commit(author.id, writeToArray(delta))
    val event    =
      buildEvent(DeltaCommitment(revealed.commitment(author.id)), author, authorPrivateKey, parents, authorization)
    (event, revealed)
  }

}
