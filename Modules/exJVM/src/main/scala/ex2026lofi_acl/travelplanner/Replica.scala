package ex2026lofi_acl.travelplanner

import crypto.PublicIdentity
import rdts.filters.PermissionTree
import rdts.time.Dot
import replication.acl.bft.Acl

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
