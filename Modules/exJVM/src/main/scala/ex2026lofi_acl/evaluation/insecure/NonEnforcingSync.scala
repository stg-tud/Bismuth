package ex2026lofi_acl.evaluation.insecure

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.channels.PrivateIdentity
import replication.acl.sync.anti_entropy.AclEnforcingSync.encoder
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots
import replication.acl.bft.{Acl, BftDelta}
import replication.acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import replication.acl.sync.anti_entropy.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}

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
