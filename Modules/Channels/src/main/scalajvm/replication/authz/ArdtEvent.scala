package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.{Hash, PublicIdentity, Signature}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload
import replication.authz.ArdtEvent.Payload.Capability

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

    import replication.JsoniterCodecsJvm.given
    given JsonValueCodec[ArdtEvent] = JsonCodecMaker.make
