package channels.crypto

import java.security.KeyPair

object IdentityFactory {
  def createNewIdentity: Ed25519PrivateIdentity = Ed25519PrivateIdentity.generate()

  def fromSeed(seed: Array[Byte]): Ed25519PrivateIdentity = Ed25519PrivateIdentity.fromSeed(seed)

  /** JVM only. */
  def fromIdentityKey(identityKey: KeyPair): Ed25519PrivateIdentity = Ed25519PrivateIdentity(identityKey)
}
