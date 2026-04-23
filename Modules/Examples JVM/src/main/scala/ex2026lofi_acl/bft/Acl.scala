package ex2026lofi_acl.bft

import crypto.PublicIdentity
import ex2026lofi_acl.legacy.monotonic
import rdts.base.{Bottom, DecoratedLattice, Lattice}
import rdts.filters.PermissionTree

case class Acl(
    read: Map[PublicIdentity, PermissionTree] = Map.empty,
    write: Map[PublicIdentity, PermissionTree] = Map.empty,
    removed: Set[PublicIdentity] = Set.empty,
    admins: Set[PublicIdentity] = Set.empty,
) extends monotonic.Acl

object Acl {
  given aclLattice: Lattice[Acl] = {
    DecoratedLattice.filter(Lattice.derived[Acl])((base, other) =>
      if other.removed.isEmpty then base
      else
          base.copy(
            read = base.read.removedAll(other.removed),
            write = base.write.removedAll(other.removed),
            admins = base.admins.removedAll(other.removed)
          )
    )
  }

  given aclBottom: Bottom[Acl] = Bottom.derived
}
