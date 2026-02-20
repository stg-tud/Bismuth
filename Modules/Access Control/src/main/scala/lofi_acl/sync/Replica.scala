package lofi_acl.sync

import crypto.PublicIdentity
import lofi_acl.sync.monotonic.Acl
import rdts.filters.PermissionTree
import rdts.time.Dot

trait Replica[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
  def connect(remoteUser: PublicIdentity, connectionString: String): Unit
  def createInvitation: Invitation
  def currentState: RDT
  def mutateState(mutator: RDT => RDT): Unit
  def currentAcl: Acl
  def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit
  def start(): Unit
  def stop(): Unit
}
