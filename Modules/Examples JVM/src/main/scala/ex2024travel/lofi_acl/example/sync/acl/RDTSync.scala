package ex2024travel.lofi_acl.example.sync.acl

import crypto.PublicIdentity
import rdts.filters.{Operation, PermissionTree}
import rdts.time.Dot

trait RDTSync[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
  def connect(remoteUser: PublicIdentity, connectionString: String): Unit
  def connectionString: String
  def currentState: RDT
  def mutateState(mutator: RDT => RDT): Unit
  def currentAcl: Acl
  def grantPermissions(affectedUser: PublicIdentity, realm: PermissionTree, typeOfPermission: Operation): Unit
  def start(): Unit
  def stop(): Unit
}
