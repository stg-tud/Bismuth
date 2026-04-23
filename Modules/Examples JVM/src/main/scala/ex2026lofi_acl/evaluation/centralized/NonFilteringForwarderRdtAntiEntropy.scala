package ex2026lofi_acl.evaluation.centralized

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2026lofi_acl.bft.Hash
import ex2026lofi_acl.bft.HashDag.Encoder
import ex2026lofi_acl.sync.anti_entropy.{AclAntiEntropy, AntiEntropyCommunicator, SignedDelta}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots

class NonFilteringForwarderRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    localIdentity: PrivateIdentity,
    network: AntiEntropyCommunicator[State],
    aclAntiEntropy: AclAntiEntropy,
    onDeltasReceived: Dots => Unit = _ => ()
)(using Encoder[SignedDelta[State]])
    extends FilteringForwarderRdtAntiEntropy[State](localIdentity, network, aclAntiEntropy, onDeltasReceived) {

  override def receiveDeltas(
      unverifiedDeltas: Seq[SignedDelta[State]],
      filtered: Dots,
      remoteAcl: Set[Hash],
      remote: PublicIdentity
  ): Unit = {
    val (localAclHeads, localAcl) = aclAntiEntropy.currentAcl

    // Don't check anything, just apply
    applyVerifiedDeltas(remote, unverifiedDeltas)

    // If remote acl < local acl, remote filter might be too restrictive.
    if filtered.nonEmpty && remoteAcl != localAclHeads then {
      val localAclDeltas: Set[Hash] = aclAntiEntropy.currentHashDag.deltas.keySet
      if remoteAcl.subsetOf(localAclDeltas)
      then { // remote acl < local acl
        val potentiallyMissing = filtered.diff(filteredDots.get()) // Only new filtered dots
        val missing            = missingDots.updateAndGet(missing => missing.union(potentiallyMissing))
        // Note that we already sent missing ACL deltas to remote, so we can be optimistic that remote is on new ACL
        // once this request is received by remote.
        network.requestDeltas(missing, remote)
      }
    } else {                                                      // Remote acl is the same as local acl
      missingDots.updateAndGet(missing => missing.diff(filtered)) // Remove filtered from missing
      val newFiltered = filteredDots.updateAndGet(known => known.union(filtered))
    }
  }

  override protected def sendDeltasFiltered(deltas: Iterable[SignedDelta[State]], remote: PublicIdentity): Unit =
    network.sendDeltas(deltas.toSeq, Dots.empty, Set.empty, remote)
}
