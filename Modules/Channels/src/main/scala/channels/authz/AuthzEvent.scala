package channels.authz

import AuthzEvent.Payload
import AuthzEvent.Payload.Capability
import channels.crypto.{Hash, PrivateIdentity, PublicIdentity, Signature}
import channels.JsoniterCodecs.permissionTreeCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.filters.PermissionTree

case class AuthzEvent(
    payload: Payload,
    author: PublicIdentity,
    parents: Set[Hash],
    signature: Signature,
    authorization: Hash
):
    def hash: Hash = Hash.compute(writeToArray(this))

object AuthzEvent:
    given codec: JsonValueCodec[AuthzEvent] = JsonCodecMaker.make

    /** The root event: a capability with all permissions for `rootIdentity`, signed by it. */
    def createGenesis(rootIdentity: PrivateIdentity): AuthzEvent = {
      val genesisEvent = AuthzEvent(
        Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.allow),
        rootIdentity.getPublic,
        Set.empty,
        Signature.allZeroSignature,
        Hash.allZeroHash
      )
      val encoded   = writeToArray(genesisEvent)
      val signature = rootIdentity.sign(encoded)
      genesisEvent.copy(signature = signature)
    }

    enum Payload:
        case DeltaCommitment(commitment: Hash)
        case Capability(holder: PublicIdentity, read: PermissionTree, write: PermissionTree)
        case Revocation(revokedCapability: Hash)
