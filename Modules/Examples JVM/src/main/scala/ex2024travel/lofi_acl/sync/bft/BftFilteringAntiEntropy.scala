package ex2024travel.lofi_acl.sync.bft

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2024travel.lofi_acl.sync.*
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.Signature
import ex2024travel.lofi_acl.sync.bft.BftFilteringAntiEntropy.SyncMsg
import ex2024travel.lofi_acl.sync.bft.BftFilteringAntiEntropy.SyncMsg.*
import ex2024travel.lofi_acl.sync.monotonic.FilteringAntiEntropy.PartialDelta
import rdts.base.{Bottom, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.Queue
import scala.util.Random

// Responsible for enforcing ACL
class BftFilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    aclRoot: SerializedAclOp,
    syncInstance: SyncWithBftAcl[RDT],
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver)
)(using
    rdtCodec: JsonValueCodec[RDT],
    filter: Filter[RDT],
    rdtLattice: Lattice[RDT],
    rdtBottom: Bottom[RDT],
) extends MessageReceiver[SyncMsg[RDT]] {
  private val localPublicId     = localIdentity.getPublic
  private val connectionManager =
    connectionManagerProvider(localIdentity, MessageReceiver.wrap(this, mb => readFromArray[SyncMsg[RDT]](mb.asArray)))

  def connectedPeers: Set[PublicIdentity] = connectionManager.connectedPeers

  private def send(destination: PublicIdentity, syncMsg: SyncMsg[RDT]): Unit = {
    println(s"msg to ${destination.id}: $syncMsg")
    val buffer = ArrayMessageBuffer(writeToArray(syncMsg))
    connectionManager.send(destination, buffer): Unit
  }

  private def sendMultiple(destination: PublicIdentity, syncMsgs: SyncMsg[RDT]*): Unit = {
    if syncMsgs.isEmpty then return
    println(s"msgs to ${destination.id}: $syncMsgs")
    connectionManager.sendMultiple(
      destination,
      syncMsgs.map(msg => ArrayMessageBuffer(writeToArray(msg))).toArray*
    ): Unit
  }

  private def broadcast(syncMsg: SyncMsg[RDT]): Unit = {
    val buffer = ArrayMessageBuffer(writeToArray(syncMsg))
    connectionManager.broadcast(buffer): Unit
  }

  // Only updated in message queue thread
  private val peerAddressCache = AtomicReference(Set.empty[(PublicIdentity, (String, Int))])

  // RDT ----------------------
  @volatile private var rdtDeltas: DeltaMapWithPrefix[(RDT, Set[Signature])] = DeltaMapWithPrefix.empty
  @volatile private var partialDeltaStore: Map[Dot, PartialDelta[RDT]]       = Map.empty

  // Messages -----------------
  // Stores inbound messages
  val msgQueue: LinkedBlockingQueue[(SyncMsg[RDT], PublicIdentity)] = LinkedBlockingQueue()
  private var deltaMessageBacklog                                   = Queue.empty[(RdtDelta[RDT], PublicIdentity)]
  // ACL ----------------------
  // Stores deltas that couldn't be processed because of missing causal dependencies
  private var aclMessageBacklog: Map[Signature, (Set[Signature], SerializedAclOp)] = Map.empty
  private val knownMissingAclOps: AtomicReference[Set[Signature]]                  = AtomicReference(Set.empty)

  // Executed in threads from ConnectionManager, thread safe
  override def receivedMessage(msg: SyncMsg[RDT], fromUser: PublicIdentity): Unit =
    msgQueue.put(msg, fromUser)

  // Executed in thread from ConnectionManager
  override def connectionEstablished(remote: PublicIdentity): Unit = {
    val (aclOpGraph, acl) = syncInstance.currentBftAcl
    // TODO: It might make sense to tell the other side which document we're talking about. Otherwise they might try to
    //  sync unrelated op graphs
    send(remote, RequestMissingAcl(aclOpGraph.heads, knownMissingAclOps.get()))
    broadcast(AnnouncePeers(peerAddressCache.get()))
  }

  // Executed in thread from ConnectionManager
  override def connectionShutdown(remote: PublicIdentity): Unit =
    println(s"Disconnected from $remote")

  def newPeers(peers: Set[(PublicIdentity, (String, Int))]): Unit =
    receivedMessage(AnnouncePeers(peers), localPublicId)

  def removePeer(user: PublicIdentity): Unit =
      peerAddressCache.updateAndGet(cache => cache.filterNot(_._1 == user))
      connectionManager.disconnect(user)

  def mutateRdt(dot: Dot, delta: RDT): Unit = {
    require(!rdtDeltas.allDots.contains(dot))
    val (aclOpGraph, acl) = syncInstance.currentBftAcl
    val localWritePerms   = acl.write.get(localPublicId)
    if localWritePerms.isEmpty
    then
        Console.err.println("Could not mutate RDT: missing write permissions")
        return
    val filteredDelta           = filter.filter(delta, localWritePerms.get)
    val deltaMsg: RdtDelta[RDT] = RdtDelta(dot, filteredDelta, aclOpGraph.heads, aclOpGraph.heads)
    msgQueue.put((deltaMsg, localPublicId))
    broadcastFiltered(acl, deltaMsg)
  }

  def broadcastAclDelegation(delegation: SerializedAclOp): Unit = broadcast(AclDelta(delegation))

  @volatile private var stopped = false

  def stop(): Unit = {
    stopped = true
    connectionManager.shutdown()
  }

  def start(): Thread = {
    require(connectionManager.listenPort.isEmpty) // TODO: Allow restart?
    connectionManager.acceptIncomingConnections()
    peerAddressCache.updateAndGet(cache => cache + (localPublicId -> ("localhost", connectionManager.listenPort.get)))
    val thread = Thread(() =>
      while !stopped do {
        try
            receiveMessage()
        catch
            case e: InterruptedException =>
      }
    )
    thread.start()
    thread
  }

  private def receiveMessage(): Unit = {
    val (msg, sender) = msgQueue.take()

    if !syncInstance.currentBftAcl._2.removed.contains(sender) // Check if user was removed -> drop message
    then // Process message immediately or backlog it if not processable
        handleMessage(msg, sender)

        // If we processed an ACLEntry, maybe we can process backlogged messages now
        msg match {
          // We ignore causal dependencies between deltas
          case SyncMsg.AclDelta(serializedAclOp) =>
            val aclDeltaIdentifier = serializedAclOp.signatureAsString
            processDeltaBacklog()
          case _ => ()
        }
  }

  def listenPort: Option[Int] = connectionManager.listenPort

  private def handleMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit = {
    msg match
        case deltaMsg @ RdtDelta(dot, delta, authorAclHeads, senderAclHeads) =>
          if !handlePartialDelta(deltaMsg, sender)
          then deltaMessageBacklog = deltaMessageBacklog.appended((deltaMsg, sender))

        case AclDelta(encodedDelegation) =>
          val missing = syncInstance.applyAclOpIfPossible(encodedDelegation) // throws an exception if invalid signature
          // Track this delta as received (even if it has missing dependencies)
          knownMissingAclOps.updateAndGet(knownMissing => knownMissing - encodedDelegation.signatureAsString)
          if missing.nonEmpty then {
            val knownMissing = knownMissingAclOps.updateAndGet(_ union missing)
            //// TODO: Defer? Maybe save: candidate -> missing
            // send(sender, RequestMissingAcl(syncInstance.currentBftAcl._1.heads, knownMissing))
            aclMessageBacklog =
              aclMessageBacklog + (encodedDelegation.signatureAsString -> (missing, encodedDelegation))
          } else {
            processAclBacklog(Set(encodedDelegation.signatureAsString))
          }

        // heads are placed in local knownMissing if new, locally available ops specified in knownMissing of remote are sent to remote
        case RequestMissingAcl(remoteHeads: Set[Signature], knownMissing: Set[Signature]) =>
          val opGraph = syncInstance.currentBftAcl._1
          val msgs    = knownMissing
            .flatMap { sig => opGraph.ops.get(sig).map(delegation => delegation.serialize(sig)) }
            .map[AclDelta[RDT]](delegation => AclDelta(delegation))
          sendMultiple(sender, msgs.toArray*)

          val missingLocally = remoteHeads.filterNot(opGraph.ops.contains)
          if missingLocally.nonEmpty then {
            knownMissingAclOps.updateAndGet(_ union missingLocally): Unit
            //// TODO: Requesting should be deferred until after message queue is empty (+ some delay).
            // send(sender, RequestMissingAcl(syncInstance.currentBftAcl._1.heads, knownMissingAclOps.get()))
          }

        case RequestMissingRdt(remoteRdtDots) =>
          // TODO: avoid possibility of interleaving with acl change
          val (aclOpGraph, acl) = syncInstance.currentBftAcl
          val rdtDeltaCopy      = rdtDeltas

          val rdtDots          = rdtDeltas.deltaDots
          val missingRdtDeltas = rdtDots.subtract(remoteRdtDots)
          val deltas           =
            rdtDeltaCopy.retrieveDeltas(missingRdtDeltas).map[RdtDelta[RDT]] { case (dot, (delta, authorAcl)) =>
              RdtDelta(dot, delta, authorAcl, aclOpGraph.heads)
            }.toArray
          if deltas.nonEmpty then sendFiltered(sender, acl, deltas*)

          // Check if we're missing anything that the remote has
          if !rdtDeltas.deltaDots.contains(remoteRdtDots)
          then
              // TODO: We could also request deltas from those replicas that can write missing RDT deltas
              send(sender, RequestMissingRdt(rdtDots))

        case AnnouncePeers(peers) =>
          val newPeers      = peers.diff(peerAddressCache.get())
          val peerAddresses = peerAddressCache.updateAndGet(cache => cache ++ peers)
          if newPeers.nonEmpty then {
            broadcast(AnnouncePeers(peerAddresses))
          }
          val connectedPeers = connectionManager.connectedPeers
          newPeers
            .filterNot((publicId, _) => connectedPeers.contains(publicId))
            .foreach { case (user, (host, port)) =>
              connectionManager.connectTo(host, port)
            }
  }

  private def processAclBacklog(appliedOps: Set[Signature]): Unit = {
    val (ready, remaining) = aclMessageBacklog
      .map { case (sig, (missing, op)) => sig -> (missing -- appliedOps, op) }
      .partition { case (sig, (missing, op)) => missing.isEmpty }
    aclMessageBacklog = remaining
    ready.foreach { case (signature, (_, op)) =>
      val success = syncInstance.applyAclOpIfPossible(op).isEmpty
      assert(success)
    }
  }

  private def processDeltaBacklog(): Unit = {
    // Process backlogged rdt deltas
    val aclOpGraph                               = syncInstance.currentBftAcl._1
    val partialDeltaStoreBeforeProcessing        = partialDeltaStore
    val (processableDeltas, unprocessableDeltas) = deltaMessageBacklog.partition((delta, _) =>
      delta.authorAclContext.union(delta.senderAclContext).forall(aclOpGraph.ops.contains)
    )
    deltaMessageBacklog = unprocessableDeltas
    processableDeltas.foreach { (delta, sender) =>
      handlePartialDelta(delta, sender)
    }
    // if partialDeltaStore != partialDeltaStoreBeforeProcessing
    // then antiEntropyThread.interrupt() // Request missing partial deltas immediately
  }

  // Returns false if not applicable because local ACL is not superset of author and senders ACL
  private def handlePartialDelta(unfilteredDelta: RdtDelta[RDT], sender: PublicIdentity): Boolean = {
    unfilteredDelta match
        case RdtDelta(dot, _, authorAclHeads, senderAclHeads) =>
          val (aclOpGraph, localAcl) = syncInstance.currentBftAcl
          if !authorAclHeads.union(senderAclHeads).forall(aclOpGraph.ops.contains) then return false

          val authorAcl =
            if authorAclHeads == aclOpGraph.heads
            then localAcl
            else aclOpGraph.reconstruct(authorAclHeads).get
          val senderAcl =
            if senderAclHeads == aclOpGraph.heads then localAcl
            else if senderAclHeads == authorAclHeads then authorAcl
            else aclOpGraph.reconstruct(senderAclHeads).get
          val effectivePermissions = authorAcl.write(PublicIdentity(dot.place.delegate))
            .intersect(senderAcl.write(sender))
            .intersect(localAcl.read(localPublicId))

          if effectivePermissions.isEmpty then return true

          val delta = Filter[RDT].filter(unfilteredDelta.delta, effectivePermissions)

          val existingPartialDelta = partialDeltaStore.get(dot)
          if existingPartialDelta.isEmpty then {
            val requiredPermissions =
              authorAcl.write(PublicIdentity(dot.place.delegate)).intersect(localAcl.read(localPublicId))
            if requiredPermissions <= senderAcl.write(sender)
            then // Immediately applicable
                rdtDeltas = rdtDeltas.addDelta(dot, (delta, authorAclHeads))
                syncInstance.receivedDelta(dot, delta)
            else // delta is missing parts
                partialDeltaStore =
                  partialDeltaStore + (dot -> PartialDelta(delta, effectivePermissions, requiredPermissions))
          } else {
            existingPartialDelta.get match
                case PartialDelta(storedDelta, includedParts, requiredPermissions) =>
                  val combinedPermissions = includedParts.merge(effectivePermissions)
                  if requiredPermissions <= combinedPermissions
                  then // Existing partial delta merged with newly received partial delta is complete
                      val completeDelta = delta.merge(storedDelta)
                      rdtDeltas = rdtDeltas.addDelta(dot, (completeDelta, authorAclHeads))
                      syncInstance.receivedDelta(dot, completeDelta)
                      partialDeltaStore = partialDeltaStore.removed(dot)
                  else // Existing partial delta merged with newly received partial delta is not yet complete
                      partialDeltaStore = partialDeltaStore + (dot -> PartialDelta(
                        storedDelta.merge(delta),
                        combinedPermissions,
                        requiredPermissions
                      ))
          }
          true
  }

  private def sendFiltered(receiver: PublicIdentity, acl: BftAcl, deltas: RdtDelta[RDT]*): Unit = {
    val permissions = acl.read(receiver).intersect(acl.write(localPublicId))
    if permissions.isEmpty then return
    sendMultiple(
      receiver,
      deltas.map(delta => delta.copy(delta = filter.filter(delta.delta, permissions)))*
    )
  }

  private def broadcastFiltered(acl: BftAcl, delta: RdtDelta[RDT]): Unit = {
    connectionManager.connectedPeers.foreach { receiver =>
      val permissions = acl.write.getOrElse(localPublicId, PermissionTree.empty).intersect(
        acl.read.getOrElse(receiver, PermissionTree.empty)
      )
      if !permissions.isEmpty then {
        send(receiver, delta.copy(delta = filter.filter(delta.delta, permissions)))
      }
    }
  }

  private def requestPartialDeltas(): Unit = {
    val (aclOpGraph, acl) = syncInstance.currentBftAcl
    val partialDeltas     = partialDeltaStore
    val requiredPerms     =
      if partialDeltas.size < 100
      then // Only need to reconstruct what is missing
          partialDeltas.values.map(_.requiredPermissions).reduce(Lattice.merge)
      else // Request everything that is readable
          acl.read.getOrElse(localPublicId, PermissionTree.empty)

    // TODO: Could be optimized to reduce number of replicas to contact and deltas to request at the cost of complexity
    // - If a replica won't provide progress, don't request specific dot
    //    - progress can be used for this: !(goal ⋂ writer_permission <= progress)
    // - Provide existing progress to PartialReplicationPeerSubsetSolver
    // - Provide union of requiredPermissions of partial deltas instead of local read permission
    val (replicasToRequestFrom, _) =
      PartialReplicationPeerSubsetSolver.randomSubsetThatAllowsReconstruction(
        acl.write,
        requiredPerms
      )
    replicasToRequestFrom.foreach { remote =>
      sendMultiple(
        remote,
        RequestMissingRdt[RDT](rdtDeltas.allDots)
      )
    }
  }

  private def syncWithPeer(peer: PublicIdentity): Unit = {
    // If no partial deltas are stored locally, request from peer.
    // If there are partial deltas, use PartialReplicationPeerSubsetSolver to choose replicas to request from.
    val (aclOpGraph, acl) = syncInstance.currentBftAcl
    sendMultiple(
      peer,
      RequestMissingRdt(rdtDeltas.allDots),
      RequestMissingAcl(aclOpGraph.heads, knownMissingAclOps.get())
    )
  }

  private val antiEntropyThread = new Thread {
    private val rand = Random()

    override def run(): Unit = {
      while !stopped do {
        try {
          // Execute every 1 to 3 seconds, avoiding synchronization of these requests among replicas.
          // See: https://dl.acm.org/doi/10.1145/167954.166241
          val sleepAmount = 1_000 + rand.nextInt(2_000)
          Thread.sleep(sleepAmount)
        } catch {
          case _: InterruptedException => if stopped then return // Otherwise request missing deltas immediately
        }
        val peers = connectionManager.connectedPeers.toArray
        if peers.nonEmpty then
            if partialDeltaStore.isEmpty then
                val peer = peers(rand.nextInt(peers.length))
                syncWithPeer(peer)
            else
                requestPartialDeltas()
      }
    }
  }
  antiEntropyThread.start()
}

object BftFilteringAntiEntropy {
  enum SyncMsg[RDT]:
      // TODO: Add bundle of (acl, rdt)-deltas as message type
      case RdtDelta(dot: Dot, delta: RDT, authorAclContext: Set[Signature], senderAclContext: Set[Signature])
      case AclDelta(serializedAclOp: SerializedAclOp)
      case AnnouncePeers(peers: Set[(PublicIdentity, (String, Int))])
      case RequestMissingRdt(rdtDots: Dots)
      // TODO: Improve anti entropy of ACL with bloom filter/riblt based sync
      case RequestMissingAcl(aclVersion: Set[Signature], knownMissing: Set[Signature])

  object SyncMsg {
    given codec[RDT](using JsonValueCodec[RDT]): JsonValueCodec[SyncMsg[RDT]] = {
      import ex2024travel.lofi_acl.sync.JsoniterCodecs.uidKeyCodec
      JsonCodecMaker.make
    }
  }

}
