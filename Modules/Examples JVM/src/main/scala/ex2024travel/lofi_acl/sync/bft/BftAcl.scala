package ex2024travel.lofi_acl.sync.bft

import crypto.PublicIdentity
import ex2024travel.lofi_acl.sync.Acl
import rdts.filters.PermissionTree

case class BftAcl(
    read: Map[PublicIdentity, PermissionTree],
    write: Map[PublicIdentity, PermissionTree],
    removed: Set[PublicIdentity] = Set.empty
):
  def addPermissions(user: PublicIdentity, read: PermissionTree, write: PermissionTree): BftAcl = {
    if removed.contains(user) then this // Don't fail, otherwise reconstruction in reverse order would fail
    else
      copy(
        read = this.read.updatedWith(user) {
          case Some(oldRead) => Some(oldRead.merge(read))
          case None          => Some(read)
        },
        write = this.write.updatedWith(user) {
          case Some(oldWrite) => Some(oldWrite.merge(write))
          case None           => Some(write)
        }
      )
  }

  def remove(publicIdentity: PublicIdentity): BftAcl = {
    BftAcl(
      read.removed(publicIdentity),
      write.removed(publicIdentity),
      removed + publicIdentity
    )
  }

  // TODO: Remove Acl
  def asAcl: Acl = Acl(read, write)
