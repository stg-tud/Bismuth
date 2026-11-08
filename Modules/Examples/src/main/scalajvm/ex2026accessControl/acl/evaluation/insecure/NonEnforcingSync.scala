package ex2026accessControl.acl.evaluation.insecure

import channels.connection.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import channels.crypto.PrivateIdentity
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots
import ex2026accessControl.acl.sync.AclEnforcingSync.encoder
import ex2026accessControl.acl.sync.{AclAntiEntropy, AclEnforcingSync, FilteredRdtAntiEntropy}
import ex2026accessControl.acl.{Acl, BftDelta}
import ex2026accessControl.acl.sync.{ChannelConnectionManager, MessageReceiver}
import ex2026accessControl.authz.evaluation.ConnectionManager

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
