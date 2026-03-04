package lofi_acl.legacy.bft

import rdts.filters.PermissionTree

case class PartialDelta[RDT](delta: RDT, includedParts: PermissionTree, requiredPermissions: PermissionTree)
