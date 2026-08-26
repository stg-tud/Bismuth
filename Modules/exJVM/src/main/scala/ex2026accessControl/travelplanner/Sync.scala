package ex2026accessControl.travelplanner

import crypto.PublicIdentity
import rdts.filters.PermissionTree

// TODO: Probably should be refactored away
trait Sync[RDT] {
  def connect(remoteUser: PublicIdentity, connectionString: String): Unit
  def createInvitation: Invitation
  def currentState: RDT
  def mutateState(mutator: RDT => RDT): Unit
  def availablePermissions: Map[PublicIdentity, (read: PermissionTree, write: PermissionTree)]
  def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit
  def start(): Unit
  def stop(): Unit
}
