package channels

import channels.crypto.{PrivateIdentity, X509Util}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils
import rdts.base.Uid

import java.io.ByteArrayInputStream
import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.{SSLEngine, SSLSession, X509TrustManager}

/** TLS configuration for [[NioTCP.listenTls]] and [[NioTCP.connectTls]]: mutually authenticated TLS 1.3 between
  * [[PrivateIdentity]]s. Both sides present a certificate bound to their identity key (see
  * [[X509Util.generateCustomP2PX509Certificate]]), the authenticated peer replica id is the id of the remote
  * [[channels.crypto.PublicIdentity]]. Any well-formed identity is accepted; deciding which peers to trust is up to the user.
  */
class NioTls(privateIdentity: PrivateIdentity) {
  private val sslFactory = {
    val keyManager = {
      val certPemFile = new ByteArrayInputStream(privateIdentity.tlsCertPem.getBytes)
      val keyPemFile  = new ByteArrayInputStream(privateIdentity.tlsKeyPem.getBytes)
      PemUtils.loadIdentityMaterial(certPemFile, keyPemFile)
    }

    SSLFactory
      .builder()
      .withCiphers("TLS_CHACHA20_POLY1305_SHA256")
      .withProtocols("TLSv1.3")
      .withIdentityMaterial(keyManager)
      .withTrustMaterial(NioTls.trustManager)
      .withNeedClientAuthentication()
      .build()
  }

  /** A fresh engine for one connection, configured for client or server mode. */
  def createEngine(clientMode: Boolean): SSLEngine = {
    val engine = sslFactory.getSslContext.createSSLEngine()
    engine.setUseClientMode(clientMode)
    engine.setSSLParameters(sslFactory.getSslParameters)
    if !clientMode then engine.setNeedClientAuth(true)
    engine
  }

  /** The replica id of the remote side, from the session of a completed handshake.
    * Becomes [[channels.connection.ConnectionInfo.authenticatedPeer]].
    */
  def authenticatedPeer(session: SSLSession): Uid =
    val certificate = session.getPeerCertificates()(0).asInstanceOf[X509Certificate]
    Uid(X509Util.certificateToPublicIdentity(certificate).id)
}

object NioTls {

  /** Accepts exactly the certificates created by [[X509Util.generateCustomP2PX509Certificate]]: a single Ed25519
    * X.509v3 certificate that is signed by the identity key it names.
    */
  private val trustManager: X509TrustManager = new X509TrustManager {

    private def verify(chain: Array[X509Certificate]): Unit = {
      if chain.length != 1 then throw CertificateException("Only single entry certificate chain is supported")
      val certificate = chain(0)
      if "Ed25519" != certificate.getSigAlgName then
          throw CertificateException("Only Ed25519 is supported as signature algo")
      if certificate.getVersion != 3 then throw CertificateException("Only X509v3 Certificates are supported")
      // validates and verifies the certificate
      X509Util.certificateToPublicIdentity(certificate): Unit
    }

    override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = verify(chain)
    override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = verify(chain)
    override def getAcceptedIssuers: Array[X509Certificate]                                = Array.empty
  }
}
