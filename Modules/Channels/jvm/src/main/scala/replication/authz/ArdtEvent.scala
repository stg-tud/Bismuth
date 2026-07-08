package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.{Hash, PublicIdentity, Signature}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload

case class ArdtEvent[T](
    payload: Payload[T],
    author: PublicIdentity,
    parents: Set[Hash],
    signature: Signature
):
    def hash(using JsonValueCodec[T]): Hash = {
      import replication.JsoniterCodecsJvm.given
      Hash.compute(writeToArray(this))
    }

object ArdtEvent:
    enum Payload[T]:
        case DeltaCommitment(state: T)
        case Capability(holder: PublicIdentity, read: PermissionTree, write: PermissionTree)
        case Revocation(revoked: Set[Hash])
