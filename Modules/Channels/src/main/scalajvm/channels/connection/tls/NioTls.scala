package channels.connection.tls

import channels.crypto.{Ed25519Util, PrivateIdentity}
import rdts.base.Uid

import java.net.Socket
import java.security.KeyStore
import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLEngine, SSLSession, X509ExtendedTrustManager}

/** TLS configuration for [[channels.connection.NioTCP.listenTls]] and [[channels.connection.NioTCP.connectTls]]: mutually authenticated TLS 1.3 between
  * [[channels.crypto.PrivateIdentity]]s. Both sides present a certificate bound to their identity key (see
  * [[X509Util.generateCustomP2PX509Certificate]]), the authenticated peer replica id is the id of the remote
  * [[channels.crypto.PublicIdentity]]. Any well-formed identity is accepted; deciding which peers to trust is up to the user.
  *
  * Each instance uses a fresh TLS key, certified by the identity key of `privateIdentity`.
  */
class NioTls(privateIdentity: PrivateIdentity) {
  private val tlsKey      = Ed25519Util.generateNewKeyPair
  private val certificate = X509Util.generateCustomP2PX509Certificate(privateIdentity, tlsKey)

  private val sslContext = {
    // in-memory key store, the password only exists because the API requires one
    val password = Array.emptyCharArray
    val keyStore = KeyStore.getInstance("PKCS12")
    keyStore.load(null, null)
    keyStore.setKeyEntry("tls", tlsKey.getPrivate, password, Array(certificate))
    val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    keyManagerFactory.init(keyStore, password)

    val context = SSLContext.getInstance("TLSv1.3")
    context.init(keyManagerFactory.getKeyManagers, Array(NioTls.trustManager), null)
    context
  }

  /** A fresh engine for one connection, configured for client or server mode. */
  def createEngine(clientMode: Boolean): SSLEngine = {
    val engine = sslContext.createSSLEngine()
    engine.setUseClientMode(clientMode)
    engine.setEnabledProtocols(Array("TLSv1.3"))
    engine.setEnabledCipherSuites(Array("TLS_CHACHA20_POLY1305_SHA256"))
    engine.setNeedClientAuth(true)
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
  private val trustManager: X509ExtendedTrustManager = new X509ExtendedTrustManager {

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

    // no host names or other connection specific checks, the certificate only binds the TLS key to an identity
    override def checkClientTrusted(chain: Array[X509Certificate], authType: String, socket: Socket): Unit =
      verify(chain)
    override def checkServerTrusted(chain: Array[X509Certificate], authType: String, socket: Socket): Unit =
      verify(chain)
    override def checkClientTrusted(chain: Array[X509Certificate], authType: String, engine: SSLEngine): Unit =
      verify(chain)
    override def checkServerTrusted(chain: Array[X509Certificate], authType: String, engine: SSLEngine): Unit =
      verify(chain)
  }
}
