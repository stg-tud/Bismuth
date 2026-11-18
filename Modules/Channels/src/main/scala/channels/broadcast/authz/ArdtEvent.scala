package channels.broadcast.authz

import channels.JsoniterCodecsJvm.ardtEventCodec
import channels.broadcast.authz.ArdtEvent.Payload
import channels.broadcast.authz.ArdtEvent.Payload.Capability
import channels.crypto.{Hash, PublicIdentity, Signature}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import rdts.filters.PermissionTree

case class ArdtEvent(
    payload: Payload,
    author: PublicIdentity,
    parents: Set[Hash],
    signature: Signature,
    authorization: Hash
):
    def hash: Hash = Hash.compute(writeToArray(this))

object ArdtEvent:
    enum Payload:
        case DeltaCommitment(commitment: Hash)
        case Capability(holder: PublicIdentity, read: PermissionTree, write: PermissionTree)
        case Revocation(revokedCapability: Hash)
