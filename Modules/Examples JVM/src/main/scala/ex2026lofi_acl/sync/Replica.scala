package ex2026lofi_acl.sync

import crypto.PublicIdentity
import ex2026lofi_acl.legacy.monotonic.Acl
import ex2026lofi_acl.travelplanner.Invitation
import rdts.filters.PermissionTree
import rdts.time.Dot

// TODO: Probably should be refactored away
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
