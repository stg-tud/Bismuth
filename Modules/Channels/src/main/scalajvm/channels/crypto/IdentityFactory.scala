package channels.crypto

import java.security.KeyPair

object IdentityFactory {
  def createNewIdentity: Ed25519PrivateIdentity = fromIdentityKey(Ed25519Util.generateNewKeyPair)

  def fromIdentityKey(identityKey: KeyPair): Ed25519PrivateIdentity = Ed25519PrivateIdentity(identityKey)
}
