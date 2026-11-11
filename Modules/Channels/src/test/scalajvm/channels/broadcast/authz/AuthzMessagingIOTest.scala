package channels.broadcast.authz

import channels.broadcast.authz.AuthzProtocol.*
import channels.broadcast.authz.AuthzTestSupport.{*, given}
import channels.connection.{Abort, ConnectionDescriptor, NioTCP, NioTls}
import channels.crypto.PrivateIdentity
import channels.{MessagingIO, Timers}
import munit.FunSuite
import rdts.filters.PermissionTree

import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.duration.DurationInt

/** [[AuthzProtocol]] driven by [[MessagingIO]] over mutually authenticated TLS connections. */
class AuthzMessagingIOTest extends FunSuite {

  private def eventually(cond: => Boolean, clue: => String): Unit = {
    val deadline = System.nanoTime() + 10.seconds.toNanos
    while !cond && System.nanoTime() < deadline do Thread.sleep(10)
    assert(cond, clue)
  }

  test("replicas sync events and readable deltas over TLS") {
    val abort   = Abort()
    val timer   = java.util.Timer(true)
    val root    = newPrivateIdentity()
    val bob     = newPrivateIdentity()
    val genesis = Authorization.createGenesis(root)

    def node(identity: PrivateIdentity): (MessagingIO[AuthzProtocol[Set[Int]]], NioTCP, NioTls) = {
      val nio    = NioTCP()
      val thread = Thread(() => nio.loopSelection(abort))
      thread.setDaemon(true)
      thread.start()
      val io = AuthzProtocol.messagingIO[Set[Int]](
        identity,
        genesis.hash,
        _ => (),
        MessagingIO.Config(globalAbort = abort, timers = Timers.OnJavaTimer(timer))
      )
      (io, nio, NioTls(identity))
    }

    try {
      val (a, aNio, aTls) = node(root)
      val (b, bNio, bTls) = node(bob)
      a.receiveLocally(genesis)

      a.addServerConnection(aNio.listenTls(
        aNio.defaultServerSocketChannel(InetSocketAddress(InetAddress.getLoopbackAddress, 0)),
        aTls
      ))
      eventually(a.selfConnectionDescriptors.nonEmpty, "a listens")
      val ConnectionDescriptor.TcpTls(host, port) = a.selfConnectionDescriptors.head: @unchecked
      b.addClientConnection(bNio.connectTls(bNio.defaultSocketChannel(InetSocketAddress(host, port)), bTls))

      eventually(b.replica.heads == Set(genesis.hash), s"b bootstraps: ${b.replica.heads}")

      a.mutate(_ => Set(1))
      eventually(b.replica.heads == a.replica.heads, s"b receives the event of a's first write: ${b.replica.heads}")
      assertEquals(b.replica.state, Set.empty[Int], "b cannot read deltas without a capability")

      a.delegate(genesis.hash, bob.getPublic, PermissionTree.allow, PermissionTree.allow)
      a.mutate(_ => Set(2))
      // the delegation grants read access to all deltas, including those written before it
      eventually(b.replica.state == Set(1, 2), s"b reads all deltas after delegation: ${b.replica.state}")

      b.mutate(_ => Set(3))
      eventually(a.replica.state == Set(1, 2, 3), s"a receives b's write: ${a.replica.state}")
    } finally {
      abort.abort()
      timer.cancel()
    }
  }
}
