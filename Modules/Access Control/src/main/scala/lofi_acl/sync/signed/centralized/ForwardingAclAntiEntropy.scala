package lofi_acl.sync.signed.centralized

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.sync.signed.{AclAntiEntropy, AntiEntropyCommunicator}

class ForwardingAclAntiEntropy(
    id: PrivateIdentity,
    genesis: BftDelta[Acl],
    onAclChanged: (delta: Acl) => Unit,
    network: AntiEntropyCommunicator[?]
) extends AclAntiEntropy(id, genesis, onAclChanged, network) {
  override def receiveDeltas(deltas: Seq[BftDelta[Acl]], from: PublicIdentity): Unit = {
    super.receiveDeltas(deltas, from)
    network.connectedPeers.foreach { peer =>
      if peer != from then network.sendDeltas(deltas, peer)
    }
  }
}
