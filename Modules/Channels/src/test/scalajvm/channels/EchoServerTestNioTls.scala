package channels

import channels.connection.*
import channels.crypto.{IdentityFactory, PrivateIdentity}

import java.net.{InetAddress, InetSocketAddress}

class EchoServerTestNioTls extends EchoCommunicationTest(
      (ec, abort) => {
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        nioTCP.listenTls(
          nioTCP.defaultServerSocketChannel(InetSocketAddress(InetAddress.getLoopbackAddress, 0)),
          EchoServerTestNioTls.tls1
        )
      },
      (ec, abort) => {
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        NioTcpConnectionDetailsResolver(nioTCP, Some(EchoServerTestNioTls.tls2))
      }
    )

object EchoServerTestNioTls {
  val id1: PrivateIdentity = IdentityFactory.createNewIdentity
  val id2: PrivateIdentity = IdentityFactory.createNewIdentity
  val tls1: NioTls         = NioTls(id1)
  val tls2: NioTls         = NioTls(id2)
}

class NioTlsTest extends munit.FunSuite {
  import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, MessageBuffer}
  import de.rmgk.delay.Callback
  import rdts.base.Uid

  import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
  import scala.concurrent.Promise
  import scala.util.Random

  def echoTest(name: String, sizes: List[Int], useTls: Boolean = true): Unit = test(name) {
    val abort    = Abort()
    val serverId = IdentityFactory.createNewIdentity
    val clientId = IdentityFactory.createNewIdentity
    // separate selectors: the server echo blocks its selector thread until the client reads
    val nio       = new NioTCP()
    val clientNio = new NioTCP()
    List(nio, clientNio).foreach { n =>
      val thread = Thread(() => n.loopSelection(abort))
      thread.setDaemon(true)
      thread.start()
    }

    try {
      val serverSide   = Promise[Connection]()
      val serverSocket = nio.defaultServerSocketChannel(InetSocketAddress(InetAddress.getLoopbackAddress, 0))
      val listener     = if useTls then nio.listenTls(serverSocket, NioTls(serverId)) else nio.listen(serverSocket)
      var descriptor: Option[channels.connection.ConnectionDescriptor] = None
      listener.listen { conn =>
        serverSide.success(conn)
        (result => result.foreach(mb => conn.send(mb).run(_ => ()))): Callback[MessageBuffer]
      }.runIn(abort) { result => descriptor = result.toOption }

      val received = LinkedBlockingQueue[Array[Byte]]()
      val client   = Promise[Connection]()
      NioTcpConnectionDetailsResolver(clientNio, Some(NioTls(clientId))).connect(descriptor.get).get.connect { _ =>
        (result => result.foreach(mb => received.put(mb.convertToArray()))): Callback[MessageBuffer]
      }.runIn(abort)(result => client.complete(result))

      val clientConn = scala.concurrent.Await.result(client.future, scala.concurrent.duration.Duration(10, "s"))
      if useTls then assertEquals(clientConn.info.authenticatedPeer, Some(Uid(serverId.getPublic.id)))

      val messages = sizes.map(size => Random.nextBytes(size))
      messages.foreach(message => clientConn.send(ByteBufferMessageBuffer(message)).run(_ => ()))

      // the accepting side reports connections once the first message determined the protocol
      val serverConn = scala.concurrent.Await.result(serverSide.future, scala.concurrent.duration.Duration(10, "s"))
      if useTls then assertEquals(serverConn.info.authenticatedPeer, Some(Uid(clientId.getPublic.id)))
      messages.foreach { expected =>
        val got = received.poll(10, TimeUnit.SECONDS)
        assert(got != null, "timed out waiting for echo")
        assert(java.util.Arrays.equals(got, expected), s"mismatch for message of size ${expected.length}")
      }
      clientConn.close()
    } finally {
      abort.abort()
      nio.selector.wakeup()
      clientNio.selector.wakeup(): Unit
    }
  }

  echoTest("plain: messages larger than the primary buffer", List(17000, 70_000), useTls = false)
  echoTest("small messages", List(1, 100, 5))
  echoTest("empty message", List(0, 3))
  echoTest("messages larger than a TLS record", List(17000, 70_000))
  echoTest(
    "large messages are echoed in order and both sides see the authenticated peer",
    List(0, 1, 17000, 3_000_000, 100, 70_000)
  )
}
