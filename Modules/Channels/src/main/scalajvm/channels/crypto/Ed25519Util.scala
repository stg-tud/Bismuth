package channels.crypto

import java.security.*
import java.security.interfaces.EdECPrivateKey
import java.security.spec.{NamedParameterSpec, PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64

/** Ed25519 keys and signatures using the JDK (SunEC). Keys of other providers are accepted where they use the
  * standard encodings.
  */
object Ed25519Util {

  /** DER prefix of the X.509 (SubjectPublicKeyInfo) encoding of Ed25519 public keys, followed by the 32 byte key. */
  private val x509PublicKeyPrefix: Array[Byte] =
    Array(0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00).map(_.toByte)

  // The keyPairGenerator is thread safe, since the nextBytes(…) method of the SecureRandom is thread safe.
  private val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("Ed25519")

  /** Generates a new Ed25519 KeyPair.
    *
    * @return
    *   the newly generated KeyPair.
    */
  def generateNewKeyPair: KeyPair =
    keyPairGenerator.generateKeyPair()

  def base64PublicKeyBytesToPublicKey(publicKeyBytesInBase64: String): PublicKey =
    rawPublicKeyBytesToPublicKey(Base64.getDecoder.decode(publicKeyBytesInBase64))

  def rawPublicKeyBytesToPublicKey(ed25519Bytes: Array[Byte]): PublicKey = {
    require(ed25519Bytes.length == 32, s"Ed25519 public keys are 32 bytes long, got ${ed25519Bytes.length}")
    KeyFactory.getInstance("Ed25519").generatePublic(X509EncodedKeySpec(x509PublicKeyPrefix ++ ed25519Bytes))
  }

  def publicKeyToPublicKeyBytesBase64Encoded(publicKey: PublicKey): String =
    Base64.getEncoder.encodeToString(publicKeyToRawPublicKeyBytes(publicKey))

  def publicKeyToRawPublicKeyBytes(publicKey: PublicKey): Array[Byte] = {
    if "Ed25519" != publicKey.getAlgorithm && "EdDSA" != publicKey.getAlgorithm then {
      throw IllegalArgumentException(s"Expected an Ed25519 key, got ${publicKey.getAlgorithm}")
    }
    val encoded = publicKey.getEncoded
    // also rejects Ed448 keys, which use the same algorithm name in SunEC
    if encoded.length != x509PublicKeyPrefix.length + 32 || !encoded.startsWith(x509PublicKeyPrefix) then
        throw IllegalArgumentException("Not an X.509 encoded Ed25519 public key")
    encoded.drop(x509PublicKeyPrefix.length)
  }

  def privateKeyToRawPrivateKeyBytes(privateKey: PrivateKey): Array[Byte] = {
    if "Ed25519" != privateKey.getAlgorithm && "EdDSA" != privateKey.getAlgorithm then {
      throw IllegalArgumentException(s"Expected an Ed25519 key, got ${privateKey.getAlgorithm}")
    }
    // parse with SunEC to accept keys of other providers
    val parsed = KeyFactory.getInstance("Ed25519").generatePrivate(PKCS8EncodedKeySpec(privateKey.getEncoded))
    val bytes  = parsed.asInstanceOf[EdECPrivateKey].getBytes.orElseThrow()
    if bytes.length != 32 then
        throw IllegalArgumentException(s"Ed25519 private keys are 32 bytes long, got ${bytes.length}")
    bytes
  }

  def rawPrivateKeyBytesToPrivateKey(ed25519Bytes: Array[Byte]): PrivateKey =
    rawPrivateKeyBytesToKeyPair(ed25519Bytes).getPrivate

  def rawPrivateKeyBytesToKeyPair(ed25519Bytes: Array[Byte]): KeyPair = {
    require(ed25519Bytes.length == 32, s"Ed25519 private keys are 32 bytes long, got ${ed25519Bytes.length}")
    // The JDK has no API to derive the public key from a private key, but the key pair generator uses the next 32
    // random bytes as the private key.
    val fixedRandom = new SecureRandom() {
      override def nextBytes(bytes: Array[Byte]): Unit =
        require(bytes.length == ed25519Bytes.length)
        System.arraycopy(ed25519Bytes, 0, bytes, 0, bytes.length)
    }
    val generator = KeyPairGenerator.getInstance("Ed25519")
    generator.initialize(NamedParameterSpec.ED25519, fixedRandom)
    generator.generateKeyPair()
  }

  def privateKeyToPkcs8EncodedPrivateKeyBytes(ed25519PrivateKey: PrivateKey): Array[Byte] = {
    if !List("Ed25519", "EdDSA").contains(ed25519PrivateKey.getAlgorithm) then {
      throw IllegalArgumentException(s"Expected an Ed25519 or EdDSA key, got ${ed25519PrivateKey.getAlgorithm}")
    }
    if "PKCS#8" != ed25519PrivateKey.getFormat then {
      throw RuntimeException(s"privateKey does not support PKCS#8 encoding, but uses ${ed25519PrivateKey.getFormat}")
    }
    ed25519PrivateKey.getEncoded
  }

  def checkEd25519Signature(data: Array[Byte], signature: Array[Byte], signingKey: PublicKey): Boolean = {
    val verifier = Signature.getInstance("Ed25519")
    verifier.initVerify(signingKey)
    verifier.update(data)
    verifier.verify(signature)
  }

  def checkEd25519Signature(data: Array[Byte], signature: Array[Byte], signer: PublicIdentity): Boolean = {
    val verifier = Signature.getInstance("Ed25519")
    verifier.initVerify(base64PublicKeyBytesToPublicKey(signer.id))
    verifier.update(data)
    verifier.verify(signature)
  }

  def sign(data: Array[Byte], signKey: PrivateKey): Array[Byte] = {
    val verifier = Signature.getInstance("Ed25519")
    verifier.initSign(signKey)
    verifier.update(data)
    verifier.sign()
  }
}
