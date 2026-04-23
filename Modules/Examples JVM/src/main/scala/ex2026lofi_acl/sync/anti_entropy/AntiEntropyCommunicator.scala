package ex2026lofi_acl.sync.anti_entropy

import crypto.PublicIdentity
import ex2026lofi_acl.bft.{Acl, BftDelta, Hash}
import rdts.time.Dots

trait AntiEntropyCommunicator[State] {
  def sendDeltas(
      deltas: Seq[SignedDelta[State]],
      filteredDeltas: Dots,
      aclVersion: Set[Hash],
      remote: PublicIdentity
  ): Unit

  def sendDeltas(deltas: Seq[BftDelta[Acl]], remote: PublicIdentity): Unit

  def requestDeltas(deltas: Dots, remote: PublicIdentity): Unit

  def requestDeltas(hashes: Set[Hash], remote: PublicIdentity): Unit

  def tellAclVersion(heads: Set[Hash], remote: PublicIdentity): Unit

  def tellRdtVersion(dots: Dots, remote: PublicIdentity): Unit

  def connectedPeers: Set[PublicIdentity]
}
