package ex2026accessControl.acl.travelplanner

import channels.crypto.{Ed25519Util, PublicIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import ex2026accessControl.acl.AclRdt.given_JsonValueCodec_BftDelta
import ex2026accessControl.acl.travelplanner.AclSyncInvitation.base64Encoder
import ex2026accessControl.acl.{Acl, BftDelta}
import ex2026accessControl.authz.travelplanner.Invitation

import java.security.KeyPair
import java.util.Base64

case class AclSyncInvitation(
    rootOp: BftDelta[Acl],
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
) extends Invitation {
  def encode: String = {
    val encodedRootOp = base64Encoder.encodeToString(writeToArray(rootOp))
    val key           = base64Encoder.encodeToString(Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate))
    "%s|%s|%s|%s".format(encodedRootOp, key, inviter.id, joinAddress)
  }
}

object AclSyncInvitation {
  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def createInvite(
      aclRootOp: BftDelta[Acl],
      inviter: PublicIdentity,
      joinAddress: String
  ): (PublicIdentity, AclSyncInvitation) = {
    val createdPrincipalId = Ed25519Util.generateNewKeyPair
    val publicIdentity     =
      PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(createdPrincipalId.getPublic))
    (publicIdentity, AclSyncInvitation(aclRootOp, createdPrincipalId, inviter, joinAddress))
  }

  def decode(inviteString: String): AclSyncInvitation = {
    val parts = inviteString.split('|')
    require(parts.length == 4)
    AclSyncInvitation(
      readFromArray(base64Decoder.decode(parts(0))),
      Ed25519Util.rawPrivateKeyBytesToKeyPair(base64Decoder.decode(parts(1))),
      PublicIdentity(parts(2)),
      parts(3)
    )
  }
}
