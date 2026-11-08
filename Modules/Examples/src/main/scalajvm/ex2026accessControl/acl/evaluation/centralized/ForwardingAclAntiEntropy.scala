package ex2026accessControl.acl.evaluation.centralized

import channels.crypto.PublicIdentity
import channels.crypto.PrivateIdentity
import ex2026accessControl.acl.sync.{AclAntiEntropy, AntiEntropyCommunicator}
import ex2026accessControl.acl.{Acl, BftDelta}

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
