package ex2026accessControl.travelplanner

import channels.connection.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import replication.authz.ArdtEvent.Payload.Capability
import replication.authz.{AntiEntropy, ArdtEvent, Replica}
import replication.sync.{ChannelConnectionManager, ConnectionManager, FullMeshControlPlane, MessageReceiver}

class SyncImpl[State: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
    private val localIdentity: PrivateIdentity,
    genesis: Hash,
    genesisEvent: Option[ArdtEvent] = None,
    onStateChange: State => Unit
) extends Sync[State] {
  private val replica: Replica[State] = {
    def connectionManagerProvider(handler: MessageReceiver[MessageBuffer]): ConnectionManager =
      ChannelConnectionManager(localIdentity, handler)
    def controlPlaneProvider(connectionManager: ConnectionManager) =
      FullMeshControlPlane(localIdentity.getPublic, connectionManager)
    def antiEntropyProvider(replica: Replica[?]) =
      AntiEntropy(replica, connectionManagerProvider, controlPlaneProvider)

    val replica = Replica[State](genesis, localIdentity, antiEntropyProvider, onStateChange)
    genesisEvent.foreach { event => replica.receiveEvent(writeToArray(event)) }
    replica
  }

  def availablePermissions: Map[crypto.PublicIdentity, (read: PermissionTree, write: PermissionTree)] =
    replica.activeCapabilities.map((k, v) =>
      k -> v.foldLeft((PermissionTree.empty, PermissionTree.empty)) {
        case ((readAcc, writeAcc), (_, Capability(_, read, write))) => (readAcc.merge(read), writeAcc.merge(write))
      }
    )

  def connect(remoteUser: crypto.PublicIdentity, connectionString: String): Unit = {
    val addrParts = connectionString.split(":")
    require(addrParts.length == 2)
    replica.connect(addrParts(0) -> addrParts(1).toInt)
  }

  def createInvitation: Invitation = {
    val listenAddress = replica.listenAddress.map(addr => s"${addr._1}:${addr._2}").get
    SyncInvitation.createInvite(genesis, localIdentity.getPublic, listenAddress)._2
  }

  def currentState: State = replica.state

  def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit =
    replica.activeCapabilities(localIdentity.getPublic).find((hash, cap) =>
      readPermission <= cap.read && writePermission <= cap.write
    ) match {
      case Some(capHash, _) => replica.createDelegation(capHash, affectedUser, readPermission, writePermission)
      case None             => throw new IllegalArgumentException("No capability with sufficient permissions found")
    }

  def mutateState(mutator: State => State): Unit = replica.mutateState(mutator)

  def start(): Unit = replica.start()

  def stop(): Unit = replica.stop()
}
