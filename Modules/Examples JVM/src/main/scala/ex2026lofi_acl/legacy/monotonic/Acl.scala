package ex2026lofi_acl.legacy.monotonic

import crypto.PublicIdentity
import rdts.filters.PermissionTree

trait Acl:
    val read: Map[PublicIdentity, PermissionTree]
    val write: Map[PublicIdentity, PermissionTree]
