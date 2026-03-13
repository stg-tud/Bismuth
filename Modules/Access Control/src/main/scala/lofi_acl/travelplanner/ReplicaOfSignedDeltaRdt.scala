package lofi_acl.travelplanner

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.legacy.monotonic
import lofi_acl.sync.*
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.travelplanner.{Invitation, SyncInvitation}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

class ReplicaOfSignedDeltaRdt[State](
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
) extends Replica[State] {

  val sync = AclEnforcingSync(localIdentity, connectionManagerProvider, aclGenesis, onDeltaReceive)

  // TODO: Not used in this instance, refactor
  override def receivedDelta(dot: Dot, rdt: State): Unit = ???

  override def connect(remoteUser: PublicIdentity, connectionString: String): Unit = {
    val remoteAddr = connectionString.split(':')
    require(remoteAddr.length == 2)
    sync.connect(remoteUser, remoteAddr(0), remoteAddr(1).toInt)
  }

  override def createInvitation: Invitation =
    SyncInvitation.createInvite(
      sync.aclRootOp,
      localIdentity.getPublic,
      s"${sync.listenAddress.get._1}:${sync.listenAddress.get._2}"
    )._2

  override def currentState: State = sync.currentState

  override def currentAcl: monotonic.Acl = sync.currentAcl

  override def mutateState(mutator: State => State): Unit = sync.mutate(mutator)

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
