package ex2026accessControl.authz.travelplanner

import channels.crypto.PublicIdentity

import java.security.KeyPair

trait Invitation {
  val inviter: PublicIdentity
  val joinAddress: String
  val identityKey: KeyPair

  def encode: String
}
