package channels.connection.tls

import channels.connection.{Abort, Connection, ConnectionDescriptor, Connector, Listener, Receive}
import channels.crypto.{Ed25519Util, PrivateIdentity}
import de.rmgk.delay.Async
import rdts.base.Uid

import java.net.Socket
import java.security.KeyStore
import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLEngine, SSLSession, X509ExtendedTrustManager}
import scala.util.{Failure, Success}

/** TLS on top of any reliable, ordered connection ([[connect]], [[listen]]): mutually authenticated TLS 1.3 between
  * [[channels.crypto.PrivateIdentity]]s. Both sides present a certificate bound to their identity key (see
  * [[X509Util.generateCustomP2PX509Certificate]]), the authenticated peer replica id is the id of the remote
  * [[channels.crypto.PublicIdentity]]. Any well-formed identity is accepted; deciding which peers to trust is up to the user.
  *
  * Each instance uses a fresh TLS key, certified by the identity key of `privateIdentity`.
  */
class Tls(privateIdentity: PrivateIdentity) {
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
    context.init(keyManagerFactory.getKeyManagers, Array(Tls.trustManager), null)
    context
  }

  /** Runs TLS (as client) on the connections of `inner`. They are reported once the handshake is done. */
  def connect(inner: Connector): Connector = new Connector {
    override def connect(receive: Receive): Async[Abort, Connection] = Async.fromCallback { abort ?=>
      val handler = Async.handler[Connection]
      val session = TlsSession(createEngine(clientMode = true), Tls.this, receive, handler.complete)
      inner.connect(conn => session.start(conn)).runIn(abort) {
        case Failure(ex) => handler.fail(ex)
        case Success(_)  => ()
      }
    }
  }

  /** Runs TLS (as server) on the connections accepted by `inner`. They are reported once the handshake is done. */
  def listen(inner: Listener): Listener = new Listener {
    override def listen(receive: Receive): Async[Abort, ConnectionDescriptor] =
      inner.listen(conn => TlsSession(createEngine(clientMode = false), Tls.this, receive, _ => ()).start(conn))
        .map(Tls.describe)
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

object Tls {

  /** How a connection described by `descriptor` is described with TLS on top. */
  def describe(descriptor: ConnectionDescriptor): ConnectionDescriptor = descriptor match
      case ConnectionDescriptor.Tcp(host, port)          => ConnectionDescriptor.TcpTls(host, port)
      case ConnectionDescriptor.TcpWebSocket(host, port) => ConnectionDescriptor.TcpTls(host, port)
      case other                                         => other

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
