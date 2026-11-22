package channels

import channels.connection.*
import channels.connection.tls.Tls
import channels.crypto.{IdentityFactory, PrivateIdentity}
import de.rmgk.delay.Callback
import rdts.base.Uid

import java.net.http.HttpClient
import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.Random

object TlsTest {
  val serverId: PrivateIdentity = IdentityFactory.createNewIdentity
  val clientId: PrivateIdentity = IdentityFactory.createNewIdentity
  val serverTls: Tls            = Tls(serverId)
  val clientTls: Tls            = Tls(clientId)

  def nio(ec: scala.concurrent.ExecutionContext, abort: Abort): NioTCP = {
    val nioTCP = NioTCP()
    ec.execute(() => nioTCP.loopSelection(abort))
    nioTCP
  }

  def loopback(nioTCP: NioTCP): Listener =
    nioTCP.listen(nioTCP.defaultServerSocketChannel(InetSocketAddress(InetAddress.getLoopbackAddress, 0)))

  /** Connects with TLS over a websocket of the JDK client to a TLS listener (on NioTCP). */
  val overWebSocket: ChannelResolver = {
    case ConnectionDescriptor.TcpTls(host, port) =>
      val uri = URI.create(s"ws://${ConnectionDescriptor.urlHost(host)}:$port/")
      Some(clientTls.connect(JavaWebSocketClient.connect(HttpClient.newHttpClient(), uri)))
    case _ => None
  }
}

class EchoServerTestTlsOverWebSocket extends EchoCommunicationTest(
      (ec, abort) => TlsTest.serverTls.listen(TlsTest.loopback(TlsTest.nio(ec, abort))),
      (_, _) => TlsTest.overWebSocket
    )

class TlsTest extends munit.FunSuite {

  test("large messages over a websocket are echoed in order and both sides see the authenticated peer") {
    val abort = Abort()
    try {
      val serverSide = Promise[Connection]()
      val descriptor = Promise[ConnectionDescriptor]()
      TlsTest.serverTls.listen(TlsTest.loopback(TlsTest.nio(scala.concurrent.ExecutionContext.global, abort))).listen {
        conn =>
          serverSide.success(conn)
          (result => result.foreach(mb => conn.send(mb).run(_ => ()))): Callback[MessageBuffer]
      }.runIn(abort)(descriptor.complete)

      val received = LinkedBlockingQueue[Array[Byte]]()
      val client   = Promise[Connection]()
      TlsTest.overWebSocket.connect(Await.result(descriptor.future, 10.seconds)).get.connect { _ =>
        (result => result.foreach(mb => received.put(mb.convertToArray()))): Callback[MessageBuffer]
      }.runIn(abort)(client.complete)

      val clientConn = Await.result(client.future, 10.seconds)
      assertEquals(clientConn.info.authenticatedPeer, Some(Uid(TlsTest.serverId.getPublic.id)))

      val messages = List(0, 1, 17000, 3_000_000, 100, 70_000).map(size => Random.nextBytes(size))
      messages.foreach(message => clientConn.send(ByteBufferMessageBuffer(message)).run(_ => ()))

      val serverConn = Await.result(serverSide.future, 10.seconds)
      assertEquals(serverConn.info.authenticatedPeer, Some(Uid(TlsTest.clientId.getPublic.id)))
      messages.foreach { expected =>
        val got = received.poll(10, TimeUnit.SECONDS)
        assert(got != null, "timed out waiting for echo")
        assert(java.util.Arrays.equals(got, expected), s"mismatch for message of size ${expected.length}")
      }
      clientConn.close()
    } finally abort.abort()
  }
}
