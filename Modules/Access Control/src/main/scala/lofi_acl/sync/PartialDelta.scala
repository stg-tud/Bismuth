package lofi_acl.sync

import rdts.filters.PermissionTree

case class PartialDelta[RDT](delta: RDT, includedParts: PermissionTree, requiredPermissions: PermissionTree)
