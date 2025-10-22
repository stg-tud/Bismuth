package ex2024travel.lofi_acl.sync.bft

import crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.sync.Invitation
import ex2024travel.lofi_acl.sync.bft.BftInvitation.base64Encoder

import java.security.KeyPair
import java.util.Base64

case class BftInvitation(
    aclRootOp: SerializedAclOp,
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
) extends Invitation {
  def encode: String = "%s|%s|%s|%s|%s".format(
    base64Encoder.encodeToString(aclRootOp.signature),
    base64Encoder.encodeToString(aclRootOp.op),
    base64Encoder.encodeToString(Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate)),
    inviter.id,
    joinAddress
  )
}

object BftInvitation {
  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def createInvite(
      bftAclOpGraph: BftAclOpGraph,
      inviter: PublicIdentity,
      joinAddress: String
  ): (PublicIdentity, BftInvitation) = {
    val rootOp             = bftAclOpGraph.ops(bftAclOpGraph.root).serialize(bftAclOpGraph.root)
    val createdPrincipalId = Ed25519Util.generateNewKeyPair
    val publicIdentity     =
      PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(createdPrincipalId.getPublic))
    (publicIdentity, BftInvitation(rootOp, createdPrincipalId, inviter, joinAddress))
  }

  def decode(inviteString: String): BftInvitation = {
    val parts = inviteString.split('|')
    require(parts.length == 5)
    BftInvitation(
      SerializedAclOp(base64Decoder.decode(parts(0)), base64Decoder.decode(parts(1))),
      Ed25519Util.rawPrivateKeyBytesToKeyPair(base64Decoder.decode(parts(2))),
      PublicIdentity(parts(3)),
      parts(4)
    )
  }
}
