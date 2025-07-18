package ex2024travel.lofi_acl.example.sync.acl.monotonic

import crypto.PublicIdentity
import rdts.filters.{Operation, PermissionTree}
import rdts.time.{Dot, Dots}

object MonotonicAclSyncMessage {
  case class Signature(sig: Array[Byte])
}

enum MonotonicAclSyncMessage[RDT]:
  case PermissionsInUse(minimumAclVersion: Dots, writePermission: PermissionTree)
  case AnnouncePeers(peers: Set[(PublicIdentity, (String, Int))])
  case AclDelta(
      subject: PublicIdentity,
      realm: PermissionTree,
      operation: Operation,
      dot: Dot, // Also indicates authorship
      cc: Dots,
      signature: MonotonicAclSyncMessage.Signature | Null
  )
  case Delta(delta: RDT, dot: Dot /* dot.place is the author */, aclCC: Dots)
  case RequestMissing(rdtDots: Dots, aclDots: Dots)
