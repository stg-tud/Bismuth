package channels.crypto

import channels.Platform
import channels.crypto.jvm.Ed25519Util
import channels.crypto.vendored.Ed25519 as VendoredEd25519

/** Ed25519 verification for the platform independent crypto types: the JDK on the JVM, the vendored implementation on
  * Scala.js and Scala Native. Use with `import channels.crypto.Ed25519.given`. For JDK keys see
  * [[channels.crypto.jvm.Ed25519Util]].
  */
object Ed25519 {

  given verifier: Verifier with {
    def verify(signer: PublicIdentity, data: Array[Byte], signature: Signature): Boolean =
      if Platform.isJvm then Ed25519Util.checkEd25519Signature(data, signature.toArray, signer)
      else VendoredEd25519.verify(signer.rawPublicKeyBytes, data, signature.toArray)
  }
}
