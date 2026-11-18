package channels.connection.tls

import channels.connection.tls.X509Util.toPem
import channels.crypto.{Ed25519PrivateIdentity, Ed25519Util, PrivateIdentity, PublicIdentity, Signature}
import munit.FunSuite
import org.bouncycastle.asn1.x500.style.BCStyle
import org.bouncycastle.cert.X509CertificateHolder
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.operator.bc.BcEdDSAContentVerifierProviderBuilder
import org.junit.Assert.assertArrayEquals

import java.io.ByteArrayInputStream
import java.security.Security
import java.security.cert.{CertificateException, CertificateFactory, X509Certificate}
import java.util.Base64

class X509UtilTest extends FunSuite {
  if Security.getProvider("BC") == null then Security.addProvider(BouncyCastleProvider()): Unit

  test("generateCustomP2PX509Certificate should have correct content, checked with Bouncy Castle") {
    val identityKey = Ed25519Util.generateNewKeyPair
    val certKey     = Ed25519Util.generateNewKeyPair
    val certHolder  = X509CertificateHolder(X509Util.generateCustomP2PX509Certificate(identityKey, certKey).getEncoded)

    assertEquals(certHolder.getVersionNumber, 3)

    // Is signed by identity
    val identityPublicKeyParam =
      Ed25519PublicKeyParameters(Ed25519Util.publicKeyToRawPublicKeyBytes(identityKey.getPublic))
    val certVerifier = new BcEdDSAContentVerifierProviderBuilder().build(identityPublicKeyParam)
    assert(certHolder.isSignatureValid(certVerifier))

    // Certifies the TLS key
    assertArrayEquals(certHolder.getSubjectPublicKeyInfo.getEncoded, certKey.getPublic.getEncoded)

    assertEquals(certHolder.getSubject, certHolder.getIssuer)
    val rdns = certHolder.getSubject.getRDNs(BCStyle.UNIQUE_IDENTIFIER)
    assertEquals(rdns.length, 1)
    assertEquals(rdns(0).getTypesAndValues.length, 1)
    val base64EncodedUnescapedIdentityPubKey = rdns(0).getFirst.getValue.toString
    val pubKeyBytes                          = Base64.getDecoder.decode(base64EncodedUnescapedIdentityPubKey)
    assertArrayEquals(pubKeyBytes, identityPublicKeyParam.getEncoded)
  }

  test("certificateToPublicIdentity should return identity") {
    val identityKey = Ed25519Util.generateNewKeyPair
    val certKey     = Ed25519Util.generateNewKeyPair
    val certificate = X509Util.generateCustomP2PX509Certificate(identityKey, certKey)
    assertEquals(
      X509Util.certificateToPublicIdentity(certificate).id,
      Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
    )
  }

  test("certificateToPublicIdentity should reject certificates not signed by the named identity") {
    val identity = Ed25519PrivateIdentity(Ed25519Util.generateNewKeyPair)
    val other    = Ed25519PrivateIdentity(Ed25519Util.generateNewKeyPair)
    val impostor = new PrivateIdentity {
      def getPublic: PublicIdentity          = identity.getPublic
      def sign(data: Array[Byte]): Signature = other.sign(data)
    }
    val certificate = X509Util.generateCustomP2PX509Certificate(impostor, Ed25519Util.generateNewKeyPair)
    intercept[CertificateException](X509Util.certificateToPublicIdentity(certificate))
  }

  test("toPem should return parseable certificate in PEM format") {
    val identityKey = Ed25519Util.generateNewKeyPair
    val certKey     = Ed25519Util.generateNewKeyPair
    val certificate = X509Util.generateCustomP2PX509Certificate(identityKey, certKey)
    val pemString   = certificate.toPem.pemString

    List("BC", "SUN").foreach { providerName =>
      val cf         = CertificateFactory.getInstance("X.509", providerName)
      val parsedCert = cf.generateCertificate(ByteArrayInputStream(pemString.getBytes()))
      assert(parsedCert.isInstanceOf[X509Certificate])
      assertArrayEquals(parsedCert.getEncoded, certificate.getEncoded)
    }
  }
}
