package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft
import lofi_acl.bft.*
import lofi_acl.bft.AclRdt.given_Encoder_BftDelta
import lofi_acl.bft.HashDag.Hashable
import rdts.base.Bottom

import java.util.concurrent.atomic.AtomicReference

class AclAntiEntropy(
    private val id: PrivateIdentity,
    initialHashDag: HashDag[BftDelta[Acl], Acl],
    onAclChanged: (delta: Acl) => Unit,
    network: AntiEntropyCommunicator[?]
) {

  private val aclRdt = AclRdt(id, cachedAclLookup)

  @volatile private var hashDag: HashDag[BftDelta[Acl], Acl] = initialHashDag
  def currentHashDag: HashDag[BftDelta[Acl], Acl]            = hashDag
  private val currentAclRef                                  = AtomicReference((Set.empty[Hash], Bottom[Acl].empty))
  def currentAcl: (Set[Hash], Acl)                           = currentAclRef.get()

  @volatile private var deltasInBacklog                      = Set.empty[Hash]
  private val knownMissingDeltas: AtomicReference[Set[Hash]] = AtomicReference(Set.empty)
  @volatile private var backlog: Seq[(Hash, BftDelta[Acl])]  = Vector.empty

  private def cachedAclLookup(heads: Set[Hash]): Option[Acl] = {
    val (latestHeads, acl) = currentAclRef.get()
    if latestHeads == heads then Some(acl) else None
  }

  def receiveDeltas(deltas: Seq[BftDelta[Acl]], from: PublicIdentity): Unit = {
    synchronized {
      var cumulativeDelta = Bottom[Acl].empty
      var changed         = true
      val hashes          = deltas.map(Hashable[BftDelta[Acl]].hash(_))
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
                currentAclRef.updateAndGet((_, acl) => (hashDag.heads, acl.merge(delta.state)))
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

      if changed then onAclChanged(cumulativeDelta)
    }
  }

  def updatePeerAclKnowledge(remoteHeads: Set[Hash], remote: PublicIdentity): Unit = {
    if remoteHeads == hashDag.heads then return

    HashDagSync.missingInSubsetHashDag(hashDag, remoteHeads) match {
      case Some(missing) => network.sendDeltas(missing, remote)
      case None          =>
        // which are missing locally?
        val missingLocally = remoteHeads.filterNot(hashDag.deltas.contains)
        if missingLocally.nonEmpty then {
          network.requestDeltas(
            knownMissingDeltas.updateAndGet(_ union missingLocally),
            remote
          )
        }

        // There might be deltas missing on remote, but we don't know which ones
        network.tellAclVersion(currentAcl._1, remote)
    }
  }

  def respondToDeltaRequest(missing: Set[Hash], remote: PublicIdentity): Unit = {
    val localDeltas = hashDag.deltas
    network.sendDeltas(missing.toSeq.flatMap(localDeltas.get), remote)
  }

  def mutate(delta: Acl): Unit = {
    var toDisseminate: Hash = null
    synchronized {
      hashDag = aclRdt.mutate(delta, hashDag) // Throws an exception if delta is illegal
      assert(hashDag.heads.size == 1)
      toDisseminate = hashDag.heads.head
    }

    // Notify about acl change
    onAclChanged(currentAcl._2)

    // Broadcast change
    network.connectedPeers.foreach { remote =>
      network.sendDeltas(Seq(hashDag.deltas(toDisseminate)), remote)
    }
  }
}
