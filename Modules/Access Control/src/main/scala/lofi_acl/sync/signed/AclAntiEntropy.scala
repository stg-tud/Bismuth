package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.*
import lofi_acl.bft.AclRdt.given_Encoder_SignedDelta
import lofi_acl.bft.HashDag.Hashable
import rdts.base.Bottom

import java.util.concurrent.atomic.AtomicReference

class AclAntiEntropy[RDT](private val id: PrivateIdentity, initialHashDag: HashDag[SignedDelta[Acl], Acl]) {

  private val aclRdt = AclRdt(id, cachedAclLookup)

  @volatile private var hashDag: HashDag[SignedDelta[Acl], Acl] = initialHashDag
  private val latestAcl                                         = AtomicReference((Set.empty[Hash], Bottom[Acl].empty))

  @volatile private var deltasInBacklog                           = Set.empty[Hash]
  private val knownMissingDeltas: AtomicReference[Set[Hash]]      = AtomicReference(Set.empty)
  @volatile private var backlog: Vector[(Hash, SignedDelta[Acl])] = Vector.empty

  private def cachedAclLookup(heads: Set[Hash]): Option[Acl] = {
    val (latestHeads, acl) = latestAcl.get()
    if latestHeads == heads then Some(acl) else None
  }

  def receiveDeltas(deltas: Vector[SignedDelta[Acl]], from: PublicIdentity): Unit = {
    synchronized {
      var changed    = true
      val hashes     = deltas.map(Hashable[SignedDelta[Acl]].hash(_))
      var toApply    = hashes.zip(deltas).concat(backlog)
      var newMissing = Set.empty[Hash]
      // TODO: A more efficient/robust approach would be to topologically sort the deltas
      while changed do {
        changed = false
        toApply = toApply.filter { (hash, delta) =>
          try {
            aclRdt.receiveWithComputedHash(hash, delta, hashDag) match {
              case Left(missingForDelta) =>
                newMissing = newMissing union missingForDelta
                true // Keep delta, not yet applied
              case Right(updatedHashDag) =>
                changed = true;
                hashDag = updatedHashDag
                latestAcl.updateAndGet((_, acl) => (hashDag.heads, acl.merge(delta.state)))
                false // Remove applied delta from queue
            }
          } catch {
            case e: IllegalArgumentException =>
              System.err.println(s"Illegal delta: $delta")
              false // Remove illegal delta
          }
        }
      }

      backlog = toApply
      deltasInBacklog = backlog.map(_._1).toSet
      knownMissingDeltas.updateAndGet(oldMissing => (oldMissing ++ newMissing) diff deltasInBacklog): Unit

      // TODO: Notify about changed ACL?
      // TODO: Notify about newly missing deltas?
    }
  }

  def updatePeerAclKnowledge(remoteHeads: Set[Acl], peer: PublicIdentity): Unit = {
    // Check if equal
    // -> Is less on remote?
    // -> Is advanced on remote?
  }
}
