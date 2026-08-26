package ex2026accessControl.travelplanner

import crypto.PublicIdentity
import rdts.filters.PermissionTree
import replication.acl.Acl

// TODO: Probably should be refactored away
trait Replica[RDT] {
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
