package ex2026accessControl.travelplanner

import channels.connection.{Abort, ConnectionDescriptor}
import channels.crypto.{PrivateIdentity, PublicIdentity}
import channels.{MessagingIO, NioTCP, NioTcpConnectionDetailsResolver, NioTls, Timers}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.ArdtEvent.Payload.Capability
import replication.authz.AuthzProtocol.*
import replication.authz.{ArdtEvent, AuthzProtocol}
import replication.crypto.Hash

import java.net.{InetAddress, InetSocketAddress}

/** [[Sync]] via the [[AuthzProtocol]] over mutually authenticated TLS. Peers learn about each other's addresses
  * through the full mesh overlay, and connect to all of them.
  */
class SyncImpl[State: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    private val localIdentity: PrivateIdentity,
    genesis: Hash,
    genesisEvent: Option[ArdtEvent] = None,
    onStateChange: State => Unit
) extends Sync[State] {

  private val abort = Abort()
  private val timer = java.util.Timer(true)
  private val nio   = NioTCP()
  private val tls   = NioTls(localIdentity)

  private val io: MessagingIO[AuthzProtocol[State]] = AuthzProtocol.messagingIO[State](
    localIdentity,
    genesis,
    onStateChange,
    MessagingIO.Config(
      resolver = NioTcpConnectionDetailsResolver(nio, Some(tls)),
      globalAbort = abort,
      timers = Timers.OnJavaTimer(timer),
    )
  )
  genesisEvent.foreach(io.receiveLocally)

  def availablePermissions: Map[PublicIdentity, (read: PermissionTree, write: PermissionTree)] =
    io.replica.activeCapabilities.map((k, v) =>
      k -> v.foldLeft((PermissionTree.empty, PermissionTree.empty)) {
        case ((readAcc, writeAcc), (_, Capability(_, read, write))) => (readAcc.merge(read), writeAcc.merge(write))
      }
    )

  def connect(remoteUser: PublicIdentity, connectionString: String): Unit = {
    val addrParts = connectionString.split(":")
    require(addrParts.length == 2)
    io.addClientConnection(
      nio.connectTls(nio.defaultSocketChannel(InetSocketAddress(addrParts(0), addrParts(1).toInt)), tls)
    )
  }

  def createInvitation: Invitation = {
    val listenAddress = io.selfConnectionDescriptors.collectFirst {
      case ConnectionDescriptor.TcpTls(host, port) => s"$host:$port"
    }.get
    SyncInvitation.createInvite(genesis, localIdentity.getPublic, listenAddress)._2
  }

  def currentState: State = io.replica.state

  def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit =
    io.replica.activeCapabilitiesOf(localIdentity.getPublic).find((_, cap) =>
      readPermission <= cap.read && writePermission <= cap.write
    ) match {
      case Some(capHash, _) => io.delegate(capHash, affectedUser, readPermission, writePermission)
      case None             => throw new IllegalArgumentException("No capability with sufficient permissions found")
    }

  def mutateState(mutator: State => State): Unit =
    try
      io.mutate(mutator)
    catch {
      case rex: RuntimeException => println(rex.getMessage)
    }

  def start(): Unit = {
    val selector = Thread(() => nio.loopSelection(abort))
    selector.setDaemon(true)
    selector.start()
    io.addServerConnection(
      nio.listenTls(nio.defaultServerSocketChannel(InetSocketAddress(InetAddress.getLoopbackAddress, 0)), tls)
    )
  }

  def stop(): Unit = {
    abort.abort()
    timer.cancel()
  }
}
