package lofi_acl.sync.signed

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta, HashDag}
import lofi_acl.sync.*
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dot

class ReplicaOfSignedDeltaRdt[Rdt](
    private val localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager,
    initialAclHashDag: HashDag[BftDelta[Acl], Acl],
    onDeltaReceive: Rdt => Unit = (_: Rdt) => {}, // Consumes a delta
)(using
    Lattice[Rdt],
    Bottom[Rdt],
    JsonValueCodec[Rdt],
    Filter[Rdt],
    Decompose[Rdt]
) extends Replica[Rdt] {

  val sync = AclEnforcingSync(localIdentity, connectionManagerProvider, initialAclHashDag)

  override def receivedDelta(dot: Dot, rdt: Rdt): Unit = ()

  override def connect(remoteUser: PublicIdentity, connectionString: String): Unit = {
    val remoteAddr = connectionString.split(':')
    require(remoteAddr.length == 2)
    sync.connect(remoteAddr(0), remoteAddr(1).toInt)
  }

  override def createInvitation: Invitation =
    SyncInvitation.createInvite(sync.aclRootOp, localIdentity.getPublic, s"localhost:${sync.listenPort.get}")._2

  override def currentState: Rdt = sync.currentState

  override def currentAcl: monotonic.Acl = sync.currentAcl

  override def mutateState(mutator: Rdt => Rdt): Unit = sync.mutate(mutator)

  override def grantPermissions(
      affectedUser: PublicIdentity,
      readPermission: PermissionTree,
      writePermission: PermissionTree
  ): Unit = {
    sync.delegatePermission(
      if readPermission.isEmpty then Map.empty else Map(affectedUser -> readPermission),
      if writePermission.isEmpty then Map.empty else Map(affectedUser -> writePermission),
    )
  }

  override def start(): Unit = sync.start()

  override def stop(): Unit = sync.stop()
}
