package channels.crypto

import java.security.{PrivateKey, PublicKey}

/** Ed25519 (via BouncyCastle) for the platform independent crypto types. Use with
  * `import channels.crypto.Ed25519.{*, given}`.
  */
object Ed25519 {

  given verifier: Verifier with {
    def verify(signer: PublicIdentity, data: Array[Byte], signature: Signature): Boolean =
      signature.verify(signer.publicKey, data)
  }

  def sign(content: Array[Byte], signingKey: PrivateKey): Signature =
    Signature.unsafeFromArray(Ed25519Util.sign(content, signingKey))

  extension (signature: Signature)
      def verify(signingKey: PublicKey, data: Array[Byte]): Boolean =
        Ed25519Util.checkEd25519Signature(data, signature.toArray, signingKey)

  extension (publicIdentity: PublicIdentity)
      def publicKey: PublicKey = Ed25519Util.base64PublicKeyBytesToPublicKey(publicIdentity.id)

  extension (companion: PublicIdentity.type)
      def fromPublicKey(publicKey: PublicKey): PublicIdentity =
        PublicIdentity(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(publicKey))
}
