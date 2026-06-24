package ex2026accessControl.evaluation.centralized

import channels.connection.{ByteBufferMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2026accessControl.Debug
import replication.JsoniterCodecsJvm.syncMsgCodec
import replication.acl.sync.anti_entropy.AclEnforcingSync.SyncMsg.{MyAclVersionIs, MyRdtVersionIs}
import replication.acl.sync.anti_entropy.AclEnforcingSync.{SyncMsg, encoder}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots
import replication.acl.bft.{Acl, BftDelta}
import replication.acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import replication.acl.sync.anti_entropy.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}

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
    val rdtAntiEntropy = {
      if enforceAcl
      then FilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy, onRdtChanged)
      else NonFilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy, onRdtChanged)
    }

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
