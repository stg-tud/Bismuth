package lofi_acl.sync.signed

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.bft.{Acl, Hash}
import lofi_acl.sync.SynchronizedMutableArrayBufferDeltaStore
import rdts.base.{Bottom, Decompose, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class FilteredRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    private val localIdentity: PrivateIdentity,
    private val network: AntiEntropyCommunicator[State],
    aclAntiEntropy: AclAntiEntropy,
    initialDotValue: Long = 0
)(using Encoder[SignedDelta[State]]) {
  private val dotCounter = AtomicLong(0)
  private val localUid   = Uid(localIdentity.getPublic.id)

  private val currentStateRef: AtomicReference[(Dots, State)] = AtomicReference((Dots.empty, Bottom[State].empty))
  private val deltaStore                          = SynchronizedMutableArrayBufferDeltaStore[SignedDelta[State]]()
  private val filteredDots: AtomicReference[Dots] = AtomicReference(Dots.empty)
  private val missingDots: AtomicReference[Dots]  = AtomicReference(Dots.empty)

  private def notifyStateChanged(delta: State): Unit = ???
  def currentState: (Dots, State)                    = currentStateRef.get()

  def localMutation(mutator: State => State): Unit = {
    val delta                = mutator(currentState._2)
    val (aclVersion, acl)    = aclAntiEntropy.currentAcl
    val localWritePermission = acl.write.getOrElse(localIdentity.getPublic, PermissionTree.empty)
    val decomposedDelta      =
      delta.decomposed
        .filter(delta => Filter[State].isAllowed(delta, localWritePermission)) // Enforce local write permissions
        .map(d => SignedDelta.fromDelta(localIdentity, Dot(localUid, dotCounter.getAndIncrement()), d))

    if decomposedDelta.nonEmpty then {
      decomposedDelta.foreach(d => deltaStore.put(d.dot, d))
      val recombined = decomposedDelta.map(_.payload).reduce((l, r) => Lattice.merge(l, r))
      val dots       = Dots.from(decomposedDelta.map(_.dot))
      currentStateRef.updateAndGet((oldDots, oldState) => (oldDots.union(dots), oldState.merge(recombined)))
      notifyStateChanged(delta)
      broadcastDeltasFiltered(decomposedDelta)
    }
  }

  def onAclChanged(delta: Acl): Unit = {
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
    val peers = network.connectedPeers.toArray
    val idx   = lastRequestedPeer.updateAndGet(lastIndex => (lastIndex + 1) % peers.length)
    network.requestDeltas(dots, peers(idx))
  }

  private def broadcastDeltasFiltered(deltas: Iterable[SignedDelta[State]]): Unit =
    network.connectedPeers.foreach { remote => sendDeltasFiltered(deltas, remote) }

  private def sendDeltasFiltered(deltas: Iterable[SignedDelta[State]], remote: PublicIdentity): Unit = {
    val (aclVersion, acl)         = aclAntiEntropy.currentAcl
    val receiverReadPermission    = acl.read.getOrElse(remote, PermissionTree.empty)
    val (allowedDeltas, filtered) =
      deltas.partition(delta => Filter[State].isAllowed(delta.payload, receiverReadPermission))
    network.sendDeltas(allowedDeltas.toSeq, Dots.from(filtered.map(_.dot)), aclVersion, remote)
  }

  def receiveDeltas(
      unverifiedDeltas: Seq[SignedDelta[State]],
      filtered: Dots,
      remoteAcl: Set[Hash],
      remote: PublicIdentity
  ): Unit = {
    val (localAclHeads, localAcl) = aclAntiEntropy.currentAcl

    // Check signatures and enforce ACL
    val deltas = unverifiedDeltas
      .filter(delta =>
        delta.isSignatureValid
        && Filter[State].isAllowed(
          delta.payload,
          localAcl.write.getOrElse(PublicIdentity(delta.dot.place.delegate), PermissionTree.empty)
        )
      )

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
      currentStateRef.updateAndGet((oldDots, oldState) =>
        (oldDots.union(appliedDeltas), oldState.merge(accumulatedDelta))
      )
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
      network.requestDeltas(updatedMissing, remote)
    }
  }

  def respondToDeltaRequest(missingRdtDeltas: Dots, remote: PublicIdentity): Unit =
    sendDeltasFiltered(deltaStore.getAll(missingRdtDeltas).map(_._2), remote)
    // TODO: track locally unknown missingRdtDeltas?
}
