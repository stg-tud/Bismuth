package channels.crypto

import java.security.{KeyPair, PublicKey}

// TODO: Rename
/** A local Ed25519 identity. TLS material for this identity is derived by [[channels.connection.tls.NioTls]]. */
case class PrivateIdentity(identityKey: KeyPair) {

  val getPublic: PublicIdentity = PublicIdentity(
    Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
  )

  def getPublicKey: PublicKey = identityKey.getPublic
}
