package channels.crypto

/** A local identity: holds a private key and signs on behalf of its [[PublicIdentity]]. Signing is synchronous.
  * See `Ed25519PrivateIdentity` for the JVM implementation.
  */
trait PrivateIdentity {
  def getPublic: PublicIdentity
  def sign(data: Array[Byte]): Signature
}

/** Verifies signatures created by a [[PrivateIdentity]] for its [[PublicIdentity]], e.g., `Ed25519.verifier` on the JVM. */
trait Verifier {
  def verify(signer: PublicIdentity, data: Array[Byte], signature: Signature): Boolean
}
