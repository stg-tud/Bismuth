package channels.crypto

import channels.Platform
import channels.crypto.jvm.Ed25519Util
import channels.crypto.vendored.Ed25519 as VendoredEd25519

import java.security.{KeyPair, PublicKey}

/** A local Ed25519 identity, holding the raw 32 byte private key (seed). Signs with the JDK on the JVM and with the
  * vendored RFC 8032 implementation on Scala.js and Scala Native; signatures are identical.
  *
  * The JDK key pair ([[identityKey]], [[getPublicKey]]) is only available on the JVM. TLS material for this identity is
  * derived by [[channels.connection.tls.Tls]].
  */
final class Ed25519PrivateIdentity private (seed: Array[Byte]) extends PrivateIdentity {

  /** JVM only. Derived from the seed. */
  lazy val identityKey: KeyPair = Ed25519Util.rawPrivateKeyBytesToKeyPair(seed)

  val getPublic: PublicIdentity =
    if Platform.isJvm then PublicIdentity.fromRawPublicKeyBytes(Ed25519Util.publicKeyToRawPublicKeyBytes(identityKey.getPublic))
    else PublicIdentity.fromRawPublicKeyBytes(VendoredEd25519.publicKey(seed))

  /** JVM only. */
  def getPublicKey: PublicKey = identityKey.getPublic

  def sign(data: Array[Byte]): Signature =
    if Platform.isJvm then Signature.unsafeFromArray(Ed25519Util.sign(data, identityKey.getPrivate))
    else Signature.unsafeFromArray(VendoredEd25519.sign(seed, data))

  /** The raw private key; keep secret. */
  def seedBytes: Array[Byte] = seed.clone()

  override def toString: String = s"Ed25519PrivateIdentity(${getPublic.id})"
}

object Ed25519PrivateIdentity {

  /** JVM only. */
  def apply(identityKey: KeyPair): Ed25519PrivateIdentity =
    new Ed25519PrivateIdentity(Ed25519Util.privateKeyToRawPrivateKeyBytes(identityKey.getPrivate))

  def fromSeed(seed: Array[Byte]): Ed25519PrivateIdentity = {
    require(seed.length == VendoredEd25519.SeedLength, s"Ed25519 seeds are 32 bytes long, got ${seed.length}")
    new Ed25519PrivateIdentity(seed.clone())
  }

  def generate(): Ed25519PrivateIdentity = {
    val seed = new Array[Byte](VendoredEd25519.SeedLength)
    SecureRandomBytes.fill(seed)
    new Ed25519PrivateIdentity(seed)
  }
}
