package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.{Ed25519Util, Hash, PublicIdentity, Signature}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.Capability

import java.security.{KeyPair, PrivateKey}

object AuthzTestSupport extends munit.Assertions {

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
}
