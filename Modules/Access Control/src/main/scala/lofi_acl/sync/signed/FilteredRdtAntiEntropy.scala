package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.bft.{Acl, BftDelta, Hash, HashDag}
import lofi_acl.sync.SynchronizedMutableArrayBufferDeltaStore
import rdts.base.{Bottom, Decompose, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.annotation.unused

class FilteredRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    private val localIdentity: PrivateIdentity,
    initialDotValue: Long = 0
)(using Encoder[SignedDelta[State]]) {
  private def currentAcl: (Set[Hash], Acl)                              = ???
  private def hashDag: HashDag[BftDelta[Acl], Acl]                      = ???
  private def requestDeltas(deltas: Dots, remote: PublicIdentity): Unit = ???
  private def connectedPeers: Set[PublicIdentity]                       = ???
  private def sendDeltas(
      deltas: Seq[SignedDelta[State]],
      filtered: Dots,
      acl: Set[Hash],
      remote: PublicIdentity
  ): Unit                                            = ???
  private def notifyStateChanged(delta: State): Unit = ???

  private val dotCounter = AtomicLong(0)
  private val localUid   = Uid(localIdentity.getPublic.id)

  private val latestState: AtomicReference[(Dots, State)] = AtomicReference((Dots.empty, Bottom[State].empty))
  private val deltaStore                          = SynchronizedMutableArrayBufferDeltaStore[SignedDelta[State]]()
  private val filteredDots: AtomicReference[Dots] = AtomicReference(Dots.empty)
  private val missingDots: AtomicReference[Dots]  = AtomicReference(Dots.empty)

  def localMutation(delta: State): Unit = {
    val (aclVersion, acl)    = currentAcl
    val localWritePermission = acl.write.getOrElse(localIdentity.getPublic, PermissionTree.empty)
    val decomposedDelta      =
      delta.decomposed
        .filter(delta => Filter[State].isAllowed(delta, localWritePermission)) // Enforce local write permissions
        .map(d => SignedDelta.fromDelta(localIdentity, Dot(localUid, dotCounter.getAndIncrement()), d))

    if decomposedDelta.nonEmpty then {
      decomposedDelta.foreach(d => deltaStore.put(d.dot, d))
      val recombined = decomposedDelta.map(_.payload).reduce((l, r) => Lattice.merge(l, r))
      val dots       = Dots.from(decomposedDelta.map(_.dot))
      latestState.updateAndGet((oldDots, oldState) => (oldDots.union(dots), oldState.merge(recombined)))
      broadcastDeltasFiltered(decomposedDelta)
    }
  }

  @unused
  private def onAclChange(delta: Acl): Unit = {
    // Invalidate filtered deltas, track them as missing
    if delta.read.contains(localIdentity.getPublic) then {
      val filtered = filteredDots.getAndUpdate(_ => Dots.empty)
      requestRoundRobin(missingDots.updateAndGet(missing => missing.union(filtered)))
    }
  }

  private val lastRequestedPeer: AtomicInteger    = AtomicInteger(-1)
  private def requestRoundRobin(dots: Dots): Unit = {
    if dots.isEmpty then return

    // Request missing round-robin style
    val peers = connectedPeers.toArray
    val idx   = lastRequestedPeer.updateAndGet(lastIndex => (lastIndex + 1) % peers.length)
    requestDeltas(dots, peers(idx))
  }

  private def broadcastDeltasFiltered(deltas: Iterable[SignedDelta[State]]): Unit =
    connectedPeers.foreach { remote => sendDeltasFiltered(deltas, remote) }

  private def sendDeltasFiltered(deltas: Iterable[SignedDelta[State]], remote: PublicIdentity): Unit = {
    val (aclVersion, acl)         = currentAcl
    val receiverReadPermission    = acl.read.getOrElse(remote, PermissionTree.empty)
    val (allowedDeltas, filtered) =
      deltas.partition(delta => Filter[State].isAllowed(delta.payload, receiverReadPermission))
    sendDeltas(allowedDeltas.toSeq, Dots.from(filtered.map(_.dot)), aclVersion, remote)
  }

  def receiveDeltas(
      unverifiedDeltas: Seq[SignedDelta[State]],
      filtered: Dots,
      remoteAcl: Set[Hash],
      remote: PublicIdentity
  ): Unit = {
    val (localAclHeads, localAcl) = currentAcl

    // Check signatures and enforce ACL
    val deltas = unverifiedDeltas
      .filter(delta =>
        delta.isSignatureValid
        && Filter[State].isAllowed(
          delta.payload,
          localAcl.write.getOrElse(PublicIdentity(delta.dot.place.delegate), PermissionTree.empty)
        )
      )

    // If remote acl < local acl, remote filter might be too restrictive
    if !filtered.isEmpty && remoteAcl != localAclHeads then {
      val localAclDeltas: Set[Hash] = hashDag.heads
      if remoteAcl.subsetOf(localAclDeltas) // remote acl > local acl
      then {
        // Note that we already sent missing ACL deltas to remote
        val potentiallyMissing = filtered.diff(filteredDots.get()) // Only new filtered dots
        val missing            = missingDots.updateAndGet(missing => missing.union(potentiallyMissing))
        requestDeltas(missing, remote)
      }
    } else {                                                      // Remote acl is the same as local acl
      missingDots.updateAndGet(missing => missing.diff(filtered)) // Remove filtered from missing
      val newFiltered = filteredDots.updateAndGet(known => known.union(filtered))
      assert(deltaStore.dots.intersect(newFiltered).isEmpty) // TODO: remove this check at some point
    }

    var appliedDeltas: Dots     = Dots.empty
    var accumulatedDelta: State = Bottom[State].empty
    deltas.foreach { d =>
      deltaStore.put(d.dot, d)
      appliedDeltas = appliedDeltas.add(d.dot)
      accumulatedDelta = accumulatedDelta.merge(d.payload)
    }
    // Track deltas
    if !appliedDeltas.isEmpty then {
      latestState.updateAndGet((oldDots, oldState) => (oldDots.union(appliedDeltas), oldState.merge(accumulatedDelta)))
      missingDots.updateAndGet(missing => missing.diff(appliedDeltas))
      filteredDots.updateAndGet(filtered => filtered.diff(appliedDeltas))
      notifyStateChanged(accumulatedDelta)
    }
  }

  def updatePeerDeltaKnowledge(deltas: Dots, remote: PublicIdentity): Unit = {
    val missing = deltas.diff(deltaStore.dots.union(filteredDots.get()))
    if !missing.isEmpty
    then {
      val updatedMissing = missingDots.updateAndGet(oldMissing => oldMissing.union(missing))
      requestDeltas(updatedMissing, remote)
    }
  }

  def respondToDeltaRequest(missingRdtDeltas: Dots, remote: PublicIdentity): Unit =
    sendDeltasFiltered(deltaStore.getAll(missingRdtDeltas).map(_._2), remote)
    // TODO: track locally unknown missingRdtDeltas?
}
