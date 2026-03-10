package crypto.channels

import channels.*
import crypto.PublicIdentity
import crypto.channels.{P2PX509TrustManager, X509Util}
import de.rmgk.delay.{Async, Sync}
import nl.altindag.ssl.SSLFactory
import nl.altindag.ssl.pem.util.PemUtils
import rdts.base.Uid

import java.io.*
import java.net.*
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLServerSocket, SSLSocket}
import scala.concurrent.ExecutionContext
import scala.util.Try

class P2PTls(privateIdentity: PrivateIdentity) {
  private val sslFactory = {
    val keyManager = {
      val certPemFile = new ByteArrayInputStream(privateIdentity.tlsCertPem.getBytes)
      val keyPemFile  = new ByteArrayInputStream(privateIdentity.tlsKeyPem.getBytes)
      PemUtils.loadIdentityMaterial(certPemFile, keyPemFile)
    }
    val trustManager = new P2PX509TrustManager()

    SSLFactory
      .builder()
      .withCiphers("TLS_CHACHA20_POLY1305_SHA256")
      .withProtocols("TLSv1.3")
      .withIdentityMaterial(keyManager)
      .withTrustMaterial(trustManager)
      .withNeedClientAuthentication()
      .build()
  }

  def latentListener(ifAddress: InetAddress, listenPort: Int, ec: ExecutionContext): P2PTlsListener =
    new P2PTlsListener(ifAddress, listenPort, ec)

  def latentListener(ec: ExecutionContext): P2PTlsListener = {
    val addr = InetAddress.getLoopbackAddress
    latentListener(addr, 0, ec)
  }

  def latentConnect(host: String, port: Int, ec: ExecutionContext): LatentConnection[MessageBuffer] =
    (receiver: Receive[MessageBuffer]) =>
      Async[Abort] {
        val socket = sslFactory.getSslSocketFactory
          .createSocket(host, port)
          .asInstanceOf[SSLSocket]
        val remotePublicId = startHandshake(socket).bind
        val conn           = P2PTlsConnection(socket, privateIdentity.getPublic, Uid(remotePublicId.id), receiver)
        ec.execute(() => conn.receiveLoopBlocking())
        conn
      }

  private def startHandshake(socket: SSLSocket): Async[Abort, PublicIdentity] =
    Async.fromCallback {
      socket.addHandshakeCompletedListener { ev =>
        val peerIdentity = X509Util.certificateToPublicIdentity(
          ev.getPeerCertificates()(0).asInstanceOf[X509Certificate]
        )

        Async.handler.succeed(peerIdentity)
      }

      try
        socket.startHandshake()
      catch {
        case e: IOException => Async.handler.fail(e)
      }
    }

  class P2PTlsListener private[P2PTls] (
      val ifAddress: InetAddress,
      _listenPort: Int = 0,
      executionContext: ExecutionContext
  ) extends LatentConnection[MessageBuffer] {
    require(_listenPort >= 0 && _listenPort <= 0xffff)

    private lazy val serverSocket: SSLServerSocket = sslFactory.getSslServerSocketFactory
      .createServerSocket(_listenPort, 0, ifAddress)
      .asInstanceOf[SSLServerSocket]

    /** Bind socket if not already bound and query local port.
      *
      * @return The bound local port
      */
    def listenPort: Int = serverSocket.getLocalPort

    override def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = {
      Async.fromCallback { (abort: Abort) ?=>
        try
            serverSocket // binds port if required

            executionContext.execute(() =>
                while !abort.closeRequest do {
                  try {
                    val socket = serverSocket.accept().asInstanceOf[SSLSocket]
                    startHandshake(socket).map { (abort: Abort) ?=> peerIdentity =>
                      val conn = P2PTlsConnection(socket, privateIdentity.getPublic, Uid(peerIdentity.id), receiver)
                      executionContext.execute(() => conn.receiveLoopBlocking())
                      conn
                    }.runIn(summon)(Async.handler)
                  } catch {
                    case ex: SocketException if abort.closeRequest => ()
                    case ex: Throwable                             => Async.handler.fail(ex)
                  }
                }
                Try { serverSocket.close() }: Unit
            )
        catch {
          case ioException: IOException =>
            Async.handler.fail(ioException)
        }
      }
    }

    def close(): Unit = {
      try
        serverSocket.close()
      catch {
        case _: Throwable => ()
      }
    }
  }

  private class P2PTlsConnection(
      private val socket: SSLSocket,
      private val localId: PublicIdentity,
      peerReplicaId: Uid, // TODO: change type to PublicIdentity
      receiver: Receive[MessageBuffer]
  ) extends Connection[MessageBuffer] {
    try
        socket.setOption(StandardSocketOptions.TCP_NODELAY, true)
    catch
        case _: UnsupportedOperationException =>
          println("TCP nodelay not supported on this socket")

    private val outputStream                             = DataOutputStream(socket.getOutputStream)
    private val inputStream                              = DataInputStream(socket.getInputStream)
    private lazy val receivedMessageCallback             = receiver.messageHandler(this)
    override val authenticatedPeerReplicaId: Option[Uid] = Some(peerReplicaId)

    override def info: ConnectionInfo = {
      def socketAddrToString(socketAddr: SocketAddress): String = socketAddr match {
        case address: InetSocketAddress => address.getHostString + ":" + address.getPort
        case _                          => ???
      }

      ConnectionInfo(
        "type"          -> "p2ptls",
        "remoteAddress" -> Try { socketAddrToString(socket.getRemoteSocketAddress) }.recover(_.getMessage).get,
        "localAddress"  -> Try { socketAddrToString(socket.getLocalSocketAddress) }.recover(_.getMessage).get,
        // Assumption: The listen port is fixed, the initiator port varies
        "hackyIdentifier" -> {
          val ports = List(
            authenticatedPeerReplicaId.get.delegate -> socket.getPort,
            localId.id                              -> socket.getLocalPort
          )
          // Sort tuple of ports based on replica id, should be unique for connection between two devices (though if
          // there are two replicas with the same replica id, this won't work reliably)
          "%s|%s".format(ports.minBy(_._1)._2, ports.maxBy(_._1)._2)
        }
      )
    }

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      outputStream.synchronized {
        val bytes         = message.asArray
        val messageLength = bytes.length
        outputStream.writeInt(bytes.length)
        outputStream.write(bytes)
      }
    }

    private[P2PTls] def receiveLoopBlocking()(using abort: Abort): Unit = {
      inputStream.synchronized {
        try {
          while !abort.closeRequest do
              val len   = inputStream.readInt()
              val bytes = inputStream.readNBytes(len)
              if bytes.length == len then receivedMessageCallback.succeed(ArrayMessageBuffer(bytes))
              else throw EOFException(s"Could not read $len bytes for message")
        } catch {
          case ex: Throwable =>
            try close()
            catch { case _: Throwable => () }
            receivedMessageCallback.fail(ex)
        }
      }
    }

    override def close(): Unit =
      socket.close()
  }
}
