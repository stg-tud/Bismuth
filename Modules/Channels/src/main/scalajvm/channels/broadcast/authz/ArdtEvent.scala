package channels.broadcast.authz

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import channels.crypto.PublicIdentity
import channels.crypto.{Hash, Signature}
import rdts.filters.PermissionTree
import channels.JsoniterCodecsJvm.ardtEventCodec
import channels.broadcast.authz.ArdtEvent.Payload
import channels.broadcast.authz.ArdtEvent.Payload.Capability

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
