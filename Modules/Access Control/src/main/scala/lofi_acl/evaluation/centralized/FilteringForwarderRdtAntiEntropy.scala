package lofi_acl.evaluation.centralized

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.sync.anti_entropy.{AclAntiEntropy, AntiEntropyCommunicator, FilteredRdtAntiEntropy, SignedDelta}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter

class FilteringForwarderRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    localIdentity: PrivateIdentity,
    onRdtChange: State => Unit,
    network: AntiEntropyCommunicator[State],
    aclAntiEntropy: AclAntiEntropy,
)(using Encoder[SignedDelta[State]])
    extends FilteredRdtAntiEntropy[State](localIdentity, _ => (), network, aclAntiEntropy) {

  override protected def applyVerifiedDeltas(source: PublicIdentity, deltas: Seq[SignedDelta[State]]): Unit = {
    // TODO: currently merges update into the local state
    super.applyVerifiedDeltas(source, deltas)
    // Avoid sending locally created deltas twice
    if source != localIdentity.getPublic then
        // Forward deltas unconditionally to everyone except for the source
        network
          .connectedPeers
          .filterNot(_ == source)
          .foreach { remote =>
            sendDeltasFiltered(deltas, remote)
          }
  }

}
