package ex2026accessControl.acl.evaluation.centralized

import channels.connection.{ByteBufferMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import channels.crypto.PublicIdentity
import channels.crypto.PrivateIdentity
import ex2026accessControl.acl.Debug
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots
import ex2026accessControl.acl.sync.AclCodecs.syncMsgCodec
import ex2026accessControl.acl.sync.AclEnforcingSync.SyncMsg.{MyAclVersionIs, MyRdtVersionIs}
import ex2026accessControl.acl.sync.AclEnforcingSync.{SyncMsg, encoder}
import ex2026accessControl.acl.sync.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}
import ex2026accessControl.acl.{Acl, BftDelta}
import ex2026accessControl.acl.sync.{ChannelConnectionManager, MessageReceiver}
import ex2026accessControl.authz.evaluation.ConnectionManager

class ForwardingSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id, receiver),
    aclGenesis: BftDelta[Acl],
    enforceAcl: Boolean,
    onRdtChanged: Dots => Unit = _ => ()
) extends AclEnforcingSync[State](localIdentity, connectionManagerProvider, aclGenesis, (_, _) => ()) {
  override protected def instantiateAntiEntropy(): (AclAntiEntropy, FilteredRdtAntiEntropy[State]) = {
    val aclAntiEntropy = ForwardingAclAntiEntropy(localIdentity, aclGenesis, onAclChange, comm)
    val rdtAntiEntropy =
      if enforceAcl
      then FilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy, onRdtChanged)
      else NonFilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy, onRdtChanged)

    (aclAntiEntropy, rdtAntiEntropy)
  }

  override def handleMessage(msg: AclEnforcingSync.SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.MyPeersAre(_) => ()
    case _                     => super.handleMessage(msg, remote)
  }

  override protected def onConnectionEstablished(newRemote: PublicIdentity): Unit =
      // Notify remote about local ACL state
      val aclVersionMsg = ByteBufferMessageBuffer(writeToArray(MyAclVersionIs(aclAntiEntropy.currentAcl._1)))
      // Notify remote about local RDT state
      val rdtVersionMsg = ByteBufferMessageBuffer(writeToArray(MyRdtVersionIs(rdtAntiEntropy.currentState._1)))
      // But don't tell remote about peers

      connectionManager.sendMultiple(newRemote, Array(aclVersionMsg, rdtVersionMsg))
      Debug.log(s"Relay is now connected to $newRemote")
}
