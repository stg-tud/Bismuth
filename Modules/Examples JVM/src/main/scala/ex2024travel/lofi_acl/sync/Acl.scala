package ex2024travel.lofi_acl.sync

import crypto.PublicIdentity
import rdts.filters.PermissionTree

trait Acl:
    val read: Map[PublicIdentity, PermissionTree]
    val write: Map[PublicIdentity, PermissionTree]
