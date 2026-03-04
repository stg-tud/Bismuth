package lofi_acl.travelplanner

import crypto.PublicIdentity

import java.security.KeyPair

trait Invitation {
  val inviter: PublicIdentity
  val joinAddress: String
  val identityKey: KeyPair

  def encode: String
}
