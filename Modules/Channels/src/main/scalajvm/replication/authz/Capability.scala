package replication.authz

import crypto.PublicIdentity
import rdts.filters.PermissionTree

case class Capability(holder: PublicIdentity, read: PermissionTree, write: PermissionTree)
