package ex2026accessControl.travelplanner

import channels.connection.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.{AntiEntropy, ArdtEvent, Replica}
import replication.sync.{ChannelConnectionManager, ConnectionManager, FullMeshControlPlane, MessageReceiver}

class SyncImpl[State: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    private val localIdentity: PrivateIdentity,
    genesis: Hash,
    genesisEvent: Option[ArdtEvent] = None
) extends Sync[State] {
  private val replica: Replica[State] = {
    def connectionManagerProvider(handler: MessageReceiver[MessageBuffer]): ConnectionManager =
      ChannelConnectionManager(localIdentity, handler)
    def controlPlaneProvider(connectionManager: ConnectionManager) =
      FullMeshControlPlane(localIdentity.getPublic, connectionManager)
    def antiEntropyProvider(replica: Replica[?]) =
      AntiEntropy(replica, connectionManagerProvider, controlPlaneProvider)

    val replica = Replica[State](genesis, localIdentity, antiEntropyProvider)
    genesisEvent.foreach { event => replica.receiveEvent(writeToArray(event)) }
    replica
  }

  def availablePermissions: Map[crypto.PublicIdentity, (read: PermissionTree, write: PermissionTree)] = ???

  def connect(remoteUser: crypto.PublicIdentity, connectionString: String): Unit = ???

  def createInvitation: Invitation = {
    val listenAddress = replica.listenAddress.map(addr => s"${addr._1}:${addr._2}").get
    SyncInvitation.createInvite(genesis, localIdentity.getPublic, listenAddress)._2
  }

  def currentState: State = replica.state

  def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit = ???

  def mutateState(mutator: State => State): Unit = replica.mutateState(mutator)

  def start(): Unit =
    replica.listenAddress: Unit // Forces connection manager

  def stop(): Unit = ???
}
