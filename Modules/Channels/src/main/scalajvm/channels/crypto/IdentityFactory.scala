package channels.crypto

import java.security.KeyPair

object IdentityFactory {
  def createNewIdentity: PrivateIdentity = fromIdentityKey(Ed25519Util.generateNewKeyPair)

  def fromIdentityKey(identityKey: KeyPair): PrivateIdentity = PrivateIdentity(identityKey)
}
