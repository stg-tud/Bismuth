package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft
import lofi_acl.bft.*
import lofi_acl.bft.AclRdt.given_Encoder_SignedDelta
import lofi_acl.bft.HashDag.Hashable
import rdts.base.Bottom

import java.util.concurrent.atomic.AtomicReference

class AclAntiEntropy(private val id: PrivateIdentity, initialHashDag: HashDag[SignedDelta[Acl], Acl]) {

  private val aclRdt = AclRdt(id, cachedAclLookup)

  @volatile private var hashDag: HashDag[SignedDelta[Acl], Acl] = initialHashDag
  private val latestAcl                                         = AtomicReference((Set.empty[Hash], Bottom[Acl].empty))

  @volatile private var deltasInBacklog                        = Set.empty[Hash]
  private val knownMissingDeltas: AtomicReference[Set[Hash]]   = AtomicReference(Set.empty)
  @volatile private var backlog: Seq[(Hash, SignedDelta[Acl])] = Vector.empty

  // TODO: Replace
  private def sendDeltasToRemote(deltas: Seq[SignedDelta[Acl]], remote: PublicIdentity): Unit = ???
  private def requestFromRemote(hashes: Set[Hash], remote: PublicIdentity): Unit              = ???
  private def notifyRemote(remote: PublicIdentity): Unit                                      = ???
  private def notifyAclChanged(delta: Acl): Unit                                              = ???

  private def cachedAclLookup(heads: Set[Hash]): Option[Acl] = {
    val (latestHeads, acl) = latestAcl.get()
    if latestHeads == heads then Some(acl) else None
  }

  def receiveDeltas(deltas: Seq[SignedDelta[Acl]], from: PublicIdentity): Unit = {
    synchronized {
      var cumulativeDelta = Bottom[Acl].empty
      var changed         = true
      val hashes          = deltas.map(Hashable[SignedDelta[Acl]].hash(_))
      var toApply         = hashes.zip(deltas).concat(backlog)
      var newMissing      = Set.empty[Hash]
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
                cumulativeDelta = cumulativeDelta.merge(delta.state)
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
      if newMissing.nonEmpty then {
        knownMissingDeltas.updateAndGet(oldMissing => (oldMissing ++ newMissing) diff deltasInBacklog): Unit
        // TODO: request missing
      }

      if changed then notifyAclChanged(cumulativeDelta)
    }
  }

  def updatePeerAclKnowledge(remoteHeads: Set[Hash], remote: PublicIdentity): Unit = {
    if remoteHeads != hashDag.heads then
        HashDagSync.missingInSubsetHashDag(hashDag, remoteHeads) match {
          case Some(missing) => sendDeltasToRemote(missing, remote)
          case None          =>
            // which are missing locally?
            val missingLocally = remoteHeads.filterNot(hashDag.deltas.contains)
            if missingLocally.nonEmpty then {
              requestFromRemote(
                knownMissingDeltas.updateAndGet(_ union missingLocally),
                remote
              )
            }

            // There might be deltas missing on remote, but we don't know which ones
            notifyRemote(remote)
        }
  }

  def respondToDeltaRequest(missing: Set[Hash], remote: PublicIdentity): Unit = {
    val localDeltas = hashDag.deltas
    sendDeltasToRemote(missing.toSeq.flatMap(localDeltas.get), remote)
  }
}
