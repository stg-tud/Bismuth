package lofi_acl.sync.monotonic

import crypto.{Ed25519Util, PublicIdentity}
import MonotonicInvitation.base64Encoder
import lofi_acl.sync.Invitation

import java.security.KeyPair
import java.util.Base64

case class MonotonicInvitation(
    rootOfTrust: PublicIdentity,
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
) extends Invitation {
  def encode: String =
      val privateKeyBytes = Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate)
      s"${rootOfTrust.id}|${base64Encoder.encodeToString(privateKeyBytes)}|${inviter.id}|$joinAddress"
}

object MonotonicInvitation {
  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def createInvite(
      rootOfTrust: PublicIdentity,
      inviter: PublicIdentity,
      joinAddress: String
  ): (PublicIdentity, MonotonicInvitation) = {
    val createdPrincipalId = Ed25519Util.generateNewKeyPair
    val publicIdentity     =
      PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(createdPrincipalId.getPublic))
    (publicIdentity, MonotonicInvitation(rootOfTrust, createdPrincipalId, inviter, joinAddress))
  }

  def decode(inviteString: String): MonotonicInvitation = {
    val parts = inviteString.split('|')
    require(parts.length == 4)
    MonotonicInvitation(
      PublicIdentity(parts(0)),
      Ed25519Util.rawPrivateKeyBytesToKeyPair(base64Decoder.decode(parts(1))),
      PublicIdentity(parts(2)),
      parts(3)
    )
  }
}
