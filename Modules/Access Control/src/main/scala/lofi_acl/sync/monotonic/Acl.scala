package lofi_acl.sync.monotonic

import crypto.PublicIdentity
import rdts.filters.PermissionTree

trait Acl:
    val read: Map[PublicIdentity, PermissionTree]
    val write: Map[PublicIdentity, PermissionTree]
