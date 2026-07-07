package replication.acl.sync.anti_entropy

import crypto.{Hash, PublicIdentity}
import rdts.time.Dots
import replication.acl.{Acl, BftDelta}

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
