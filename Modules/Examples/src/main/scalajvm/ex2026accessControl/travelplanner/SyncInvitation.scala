package ex2026accessControl.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.{Ed25519Util, Hash, PublicIdentity}
import ex2026accessControl.travelplanner.Invitation
import ex2026accessControl.travelplanner.SyncInvitation.base64Encoder

import java.security.KeyPair
import java.util.Base64

case class SyncInvitation(
    genesis: Hash,
    identityKey: KeyPair,
    inviter: PublicIdentity,
    joinAddress: String
) extends Invitation {
  def encode: String = {
    val encodedRootOp = base64Encoder.encodeToString(writeToArray(genesis))
    val key           = base64Encoder.encodeToString(Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate))
    "%s|%s|%s|%s".format(encodedRootOp, key, inviter.id, joinAddress)
  }
}

object SyncInvitation {
  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def createInvite(
      genesis: Hash,
      inviter: PublicIdentity,
      joinAddress: String
  ): (PublicIdentity, SyncInvitation) = {
    val createdPrincipalId = Ed25519Util.generateNewKeyPair
    val publicIdentity     =
      PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(createdPrincipalId.getPublic))
    (publicIdentity, SyncInvitation(genesis, createdPrincipalId, inviter, joinAddress))
  }

  def decode(inviteString: String): SyncInvitation = {
    val parts = inviteString.split('|')
    require(parts.length == 4)
    SyncInvitation(
      readFromArray(base64Decoder.decode(parts(0))),
      Ed25519Util.rawPrivateKeyBytesToKeyPair(base64Decoder.decode(parts(1))),
      PublicIdentity(parts(2)),
      parts(3)
    )
  }
}
