package lofi_acl.bft

import crypto.PublicIdentity
import rdts.base.{Bottom, DecoratedLattice, Lattice}
import rdts.filters.PermissionTree

case class Acl(
    read: Map[PublicIdentity, PermissionTree],
    write: Map[PublicIdentity, PermissionTree],
    removed: Set[PublicIdentity],
    admins: Set[PublicIdentity],
)

object Acl {
  given aclLattice: Lattice[Acl] = {
    import rdts.base.Lattice.mapLattice
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
