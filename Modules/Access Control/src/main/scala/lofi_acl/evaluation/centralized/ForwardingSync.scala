package lofi_acl.evaluation.centralized

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.Debug
import lofi_acl.JsoniterCodecs.syncMsgCodec
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.sync.anti_entropy.AclEnforcingSync.SyncMsg.{MyAclVersionIs, MyRdtVersionIs}
import lofi_acl.sync.anti_entropy.AclEnforcingSync.{SyncMsg, encoder}
import lofi_acl.sync.anti_entropy.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots

class ForwardingSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id, receiver),
    aclGenesis: BftDelta[Acl],
    enforceAcl: Boolean,
    onRdtChanged: (Dots, State) => Unit = (_, _: State) => ()
) extends AclEnforcingSync[State](localIdentity, connectionManagerProvider, aclGenesis, onRdtChanged) {
  override protected def instantiateAntiEntropy(): (AclAntiEntropy, FilteredRdtAntiEntropy[State]) = {
    val aclAntiEntropy = ForwardingAclAntiEntropy(localIdentity, aclGenesis, onAclChange, comm)
    val rdtAntiEntropy = {
      if enforceAcl
      then FilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy)
      else NonFilteringForwarderRdtAntiEntropy(localIdentity, comm, aclAntiEntropy)
    }

    (aclAntiEntropy, rdtAntiEntropy)
  }

  override def handleMessage(msg: AclEnforcingSync.SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.MyPeersAre(_) => ()
    case _                     => super.handleMessage(msg, remote)
  }

  override protected def onConnectionEstablished(newRemote: PublicIdentity): Unit =
      // Notify remote about local ACL state
      val aclVersionMsg = ArrayMessageBuffer(writeToArray(MyAclVersionIs(aclAntiEntropy.currentAcl._1)))
      // Notify remote about local RDT state
      val rdtVersionMsg = ArrayMessageBuffer(writeToArray(MyRdtVersionIs(rdtAntiEntropy.currentState._1)))
      // But don't tell remote about peers

      connectionManager.sendMultiple(newRemote, Array(aclVersionMsg, rdtVersionMsg))
      Debug.log(s"Relay is now connected to $newRemote")
}
