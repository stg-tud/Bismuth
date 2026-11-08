package channels.crypto

import channels.crypto.X509Util.toPem
import org.bouncycastle.cert.X509CertificateHolder

import java.security.{KeyPair, PublicKey}

// TODO: Rename
case class PrivateIdentity(identityKey: KeyPair, tlsKey: KeyPair, certificateHolder: X509CertificateHolder) {

  def tlsCertPem: CertificatePem = certificateHolder.toPem

  def tlsKeyPem: PrivateKeyPem = Ed25519Util.privateKeyToPem(tlsKey.getPrivate)

  val getPublic: PublicIdentity = PublicIdentity(
    Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
  )

  def getPublicKey: PublicKey = identityKey.getPublic
}
