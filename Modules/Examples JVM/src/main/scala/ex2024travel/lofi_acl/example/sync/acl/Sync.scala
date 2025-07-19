package ex2024travel.lofi_acl.example.sync.acl

import crypto.PublicIdentity
import rdts.time.Dot

trait Sync[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
  def connect(remoteUser: PublicIdentity, connectionString: String): Unit
  def connectionString: String
}
