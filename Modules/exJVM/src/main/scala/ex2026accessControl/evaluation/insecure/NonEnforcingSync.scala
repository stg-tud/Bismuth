package ex2026accessControl.evaluation.insecure

import channels.connection.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.channels.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots
import replication.acl.sync.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}
import replication.acl.sync.AclEnforcingSync.encoder
import replication.acl.{Acl, BftDelta}
import replication.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}

class NonEnforcingSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id, receiver),
    aclGenesis: BftDelta[Acl],
    onRdtChanged: (Dots, State) => Unit
) extends AclEnforcingSync[State](localIdentity, connectionManagerProvider, aclGenesis, onRdtChanged) {

  override protected def instantiateAntiEntropy(): (AclAntiEntropy, FilteredRdtAntiEntropy[State]) = {
    val aclAntiEntropy = NopAclAntiEntropy(localIdentity, aclGenesis, comm)
    val rdtAntiEntropy = NonEnforcingNonSigningRdtAntiEntropy[State](localIdentity, onRdtChanged, comm, aclAntiEntropy)

    (aclAntiEntropy, rdtAntiEntropy)
  }
}
