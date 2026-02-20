package lofi_acl.sync.bft

import rdts.filters.PermissionTree

case class PartialDelta[RDT](delta: RDT, includedParts: PermissionTree, requiredPermissions: PermissionTree)
