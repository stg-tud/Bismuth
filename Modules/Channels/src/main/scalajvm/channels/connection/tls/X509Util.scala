package channels.connection.tls

import channels.crypto.{Ed25519PrivateIdentity, Ed25519Util, PrivateIdentity, PublicIdentity}

import java.io.ByteArrayInputStream
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.KeyPair
import java.security.cert.{CertificateException, CertificateFactory, X509Certificate}
import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}
import java.util.Base64

object X509Util {
  /** AlgorithmIdentifier of Ed25519 (OID 1.3.101.112, no parameters). */
  private val algorithmEd25519: Array[Byte] = Der.sequence(Der.objectId(Array(0x2b, 0x65, 0x70).map(_.toByte)))

  /** OID 2.5.4.45, uniqueIdentifier (in BC called UNIQUE_IDENTIFIER). */
  private val oidUniqueIdentifier: Array[Byte] = Array(0x55, 0x04, 0x2d).map(_.toByte)

  /** Generates a new X.509 Certificate using the certificateKeyPair that is signed using the identityKeyPair. For the
    * subject of the generated certificate, the public key of the identityKeyPair is used as the UNIQUE_IDENTIFIER RDN.
    * [[https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.8 See RFC5280]]
    *
    * Note that the identityKeyPair must be an ed25519 key.
    *
    * @param certificateKeyPair
    *   The key that is used to create the certificate.
    * @param identityKeyPair
    *   The ed25519 key that is used to sign the certificate
    * @return
    *   The certificate alongside the newly generated Ed25519 keypair for the certificate.
    */
  def generateCustomP2PX509Certificate(identityKeyPair: KeyPair, certificateKeyPair: KeyPair): X509Certificate = {

    if !List("Ed25519", "EdDSA").contains(identityKeyPair.getPublic.getAlgorithm) then {
      throw IllegalArgumentException("Only Ed25519 keys are accepted as the identityKeyPair")
    }

    val identityBytes = Ed25519Util.publicKeyToRawPublicKeyBytes(identityKeyPair.getPublic)
    // Check that the identityKeyPair is in fact an Ed25519 key and not an Ed448 Key (SunEC used EdDSA algorithm identifier fo both Ed25519 and Ed448)
    if identityBytes.length != 32 then {
      throw IllegalArgumentException("Only Ed25519 keys are accepted as the identityKeyPair")
    }
    generateCustomP2PX509Certificate(Ed25519PrivateIdentity(identityKeyPair), certificateKeyPair)
  }

  /** Like [[generateCustomP2PX509Certificate(identityKeyPair:java.security.KeyPair*]], but signs with `identity`, which
    * must produce Ed25519 signatures.
    */
  def generateCustomP2PX509Certificate(identity: PrivateIdentity, certificateKeyPair: KeyPair): X509Certificate = {
    // The name is a single uniqueIdentifier RDN, with the identity as UTF-8 string (no escaping, in contrast to CN)
    val name = Der.sequence(Der.set(Der.sequence(Der.objectId(oidUniqueIdentifier), Der.utf8String(identity.getPublic.id))))

    val currentTime = ZonedDateTime.now(ZoneOffset.UTC)

    val tbsCertificate = Der.sequence(
      Der.explicit(0, Der.integer(BigInteger.TWO)), // version 3
      Der.integer(BigInteger.valueOf(System.currentTimeMillis())),
      algorithmEd25519,
      name, // issuer
      Der.sequence(time(currentTime.minusHours(1)), time(currentTime.plusYears(1))),
      name, // subject
      certificateKeyPair.getPublic.getEncoded // SubjectPublicKeyInfo
    )

    val certificate = Der.sequence(
      tbsCertificate,
      algorithmEd25519,
      Der.bitString(identity.sign(tbsCertificate).toArray)
    )

    CertificateFactory.getInstance("X.509").generateCertificate(ByteArrayInputStream(certificate))
      .asInstanceOf[X509Certificate]
  }

  /** UTCTime until 2049, GeneralizedTime afterwards, see RFC 5280 section 4.1.2.5. */
  private def time(time: ZonedDateTime): Array[Byte] =
      val utc = time.withZoneSameInstant(ZoneOffset.UTC).withNano(0)
      if utc.getYear < 2050
      then
          Der.encode(Der.Tag.UtcTime, utc.format(DateTimeFormatter.ofPattern("yyMMddHHmmss'Z'")).getBytes(StandardCharsets.US_ASCII))
      else
          Der.encode(
            Der.Tag.GeneralizedTime,
            utc.format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss'Z'")).getBytes(StandardCharsets.US_ASCII)
          )

  /** Extracts the public identity (Base64 of Ed25519 Public Key) from the certificate. Also verifies that it is
    * actually signed by public key of identity.
    *
    * @param certificate
    *   The certificate to extract the subject identity from.
    * @throws java.security.cert.CertificateException
    *   if the validation failed
    */
  @throws[CertificateException]
  def certificateToPublicIdentity(certificate: X509Certificate): PublicIdentity = {
    // Certificate still valid?
    certificate.checkValidity()

    val subject = certificate.getSubjectX500Principal
    val issuer  = certificate.getIssuerX500Principal

    if !subject.equals(issuer) then throw new CertificateException("certificate not self issued")

    // The base64 encoded public key
    val subjectUniqueIdentifier = uniqueIdentifier(subject.getEncoded)
    val pubKeyBytes             =
      try Base64.getDecoder.decode(subjectUniqueIdentifier)
      catch case e: IllegalArgumentException => throw CertificateException(e)
    val pubKey                  = Ed25519Util.rawPublicKeyBytesToPublicKey(pubKeyBytes)

    // Verify cert instead of blindly returning identity
    try
      certificate.verify(pubKey)
    catch {
      case e: Exception => throw CertificateException(e)
    }

    PublicIdentity(subjectUniqueIdentifier)
  }

  /** The value of a name that consists of exactly one uniqueIdentifier RDN with a UTF-8 string value. */
  @throws[CertificateException]
  private def uniqueIdentifier(encodedName: Array[Byte]): String =
    try
        val rdn      = Der.Reader(Der.Reader(encodedName).readAll(Der.Tag.Sequence)).readAll(Der.Tag.Set)
        val typeAndValue = Der.Reader(Der.Reader(rdn).readAll(Der.Tag.Sequence))
        if !typeAndValue.read(Der.Tag.ObjectId).sameElements(oidUniqueIdentifier) then
            throw IllegalArgumentException("not a uniqueIdentifier")
        String(typeAndValue.readAll(Der.Tag.Utf8String), StandardCharsets.UTF_8)
    catch
        case e: IllegalArgumentException =>
          throw CertificateException("Subject does not consist of a single UNIQUE_IDENTIFIER", e)

  extension (certificate: X509Certificate)
      def toPem: CertificatePem = CertificatePem(Ed25519Util.pem("CERTIFICATE", certificate.getEncoded))
}
