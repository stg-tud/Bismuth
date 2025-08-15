package ex2024travel.lofi_acl.example.sync.acl

import crypto.PublicIdentity
import rdts.filters.PermissionTree

class Acl(val read: Map[PublicIdentity, PermissionTree], val write: Map[PublicIdentity, PermissionTree]):
  def addPermissions(user: PublicIdentity, read: PermissionTree, write: PermissionTree): Acl =
    Acl(
      this.read.updatedWith(user) {
        case Some(oldRead) => Some(oldRead.merge(read))
        case None          => Some(read)
      },
      this.write.updatedWith(user) {
        case Some(oldWrite) => Some(oldWrite.merge(write))
        case None           => Some(write)
      }
    )
