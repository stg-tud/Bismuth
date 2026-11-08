package channels

import crypto.channels.{P2PX509TrustManager, PrivateIdentity, X509Util}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils
import rdts.base.Uid

import java.io.ByteArrayInputStream
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLEngine, SSLSession}

/** TLS configuration for [[NioTCP.listenTls]] and [[NioTCP.connectTls]]: mutually authenticated TLS 1.3 between
  * [[PrivateIdentity]]s. Both sides present a self-signed certificate bound to their identity key (see
  * [[P2PX509TrustManager]]), the authenticated peer replica id is the id of the remote [[crypto.PublicIdentity]].
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
      .withTrustMaterial(new P2PX509TrustManager())
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
    * Becomes [[channels.connection.Connection.authenticatedPeerReplicaId]].
    */
  def authenticatedPeer(session: SSLSession): Uid =
    val certificate = session.getPeerCertificates()(0).asInstanceOf[X509Certificate]
    Uid(X509Util.certificateToPublicIdentity(certificate).id)
}
