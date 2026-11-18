package channels.crypto

import java.security.{KeyPair, PublicKey}

/** A local Ed25519 identity. TLS material for this identity is derived by [[channels.connection.tls.NioTls]]. */
case class Ed25519PrivateIdentity(identityKey: KeyPair) extends PrivateIdentity {

  val getPublic: PublicIdentity = PublicIdentity(
    Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
  )

  def getPublicKey: PublicKey = identityKey.getPublic

  def sign(data: Array[Byte]): Signature = Ed25519.sign(data, identityKey.getPrivate)
}
