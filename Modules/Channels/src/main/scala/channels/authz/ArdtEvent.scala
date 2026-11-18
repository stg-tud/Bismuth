package channels.authz

import ArdtEvent.Payload
import ArdtEvent.Payload.Capability
import channels.crypto.{Hash, PublicIdentity, Signature}
import channels.JsoniterCodecs.permissionTreeCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
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
    given codec: JsonValueCodec[ArdtEvent] = JsonCodecMaker.make

    enum Payload:
        case DeltaCommitment(commitment: Hash)
        case Capability(holder: PublicIdentity, read: PermissionTree, write: PermissionTree)
        case Revocation(revokedCapability: Hash)
