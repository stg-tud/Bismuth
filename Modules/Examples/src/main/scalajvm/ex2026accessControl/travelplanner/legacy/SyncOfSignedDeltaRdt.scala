package ex2026accessControl.travelplanner.legacy

import channels.connection.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2026accessControl.travelplanner.{Invitation, Sync}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dots
import replication.acl.sync.AclEnforcingSync
import replication.acl.{Acl, BftDelta}
import replication.sync.{ConnectionManager, MessageReceiver}

class SyncOfSignedDeltaRdt[State](
    private val localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager,
    aclGenesis: BftDelta[Acl],
    onDeltaReceive: (Dots, State) => Unit = (_, _: State) => () // Consumes a delta
)(using
    Lattice[State],
    Bottom[State],
    JsonValueCodec[State],
    Filter[State],
    Decompose[State]
) extends Sync[State] {

  val sync: AclEnforcingSync[State] =
    AclEnforcingSync(localIdentity, connectionManagerProvider, aclGenesis, onDeltaReceive)

  override def connect(remoteUser: PublicIdentity, connectionString: String): Unit = {
    val remoteAddr = connectionString.split(':')
    require(remoteAddr.length == 2)
    sync.connect(remoteUser, remoteAddr(0), remoteAddr(1).toInt)
  }

  override def createInvitation: Invitation =
    AclSyncInvitation.createInvite(
      sync.aclRootOp,
      localIdentity.getPublic,
      s"${sync.listenAddress.get._1}:${sync.listenAddress.get._2}"
    )._2

  override def currentState: State = sync.currentState

  override def availablePermissions: Map[PublicIdentity, (read: PermissionTree, write: PermissionTree)] =
    sync.currentAcl.read.keySet.map(replica =>
      replica -> (sync.currentAcl.read(replica), sync.currentAcl.write(replica))
    ).toMap

  override def mutateState(mutator: State => State): Unit = sync.mutate(mutator)

  override def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit =
    sync.delegatePermission(
      if readPermission.isEmpty then Map.empty else Map(affectedUser -> readPermission),
      if writePermission.isEmpty then Map.empty else Map(affectedUser -> writePermission),
    )

  override def start(): Unit = sync.start()

  override def stop(): Unit = sync.stop()
}
