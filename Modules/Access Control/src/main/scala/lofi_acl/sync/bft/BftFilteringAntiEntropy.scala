package lofi_acl.sync.bft

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import BftAclOpGraph.Signature
import BftFilteringAntiEntropy.SyncMsg
import BftFilteringAntiEntropy.SyncMsg.*
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver, PartialDelta, PartialReplicationPeerSubsetSolver, SynchronizedMutableArrayBufferDeltaStore}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

// Responsible for enforcing ACL
class BftFilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    aclRoot: SerializedAclOp,
    replica: ReplicaWithBftAcl[RDT],
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    autoConnect: Boolean = true,
    random: Random = Random()
)(using
    rdtCodec: JsonValueCodec[RDT],
    filter: Filter[RDT],
    rdtLattice: Lattice[RDT],
    rdtBottom: Bottom[RDT],
) extends MessageReceiver[SyncMsg[RDT]] {
  protected val localPublicId: PublicIdentity        = localIdentity.getPublic
  protected val connectionManager: ConnectionManager =
    connectionManagerProvider(localIdentity, MessageReceiver.wrap(this, mb => readFromArray[SyncMsg[RDT]](mb.asArray)))

  def connectedPeers: Set[PublicIdentity] = connectionManager.connectedPeers

  private var thread: Option[Thread] = None

  protected def send(destination: PublicIdentity, syncMsg: SyncMsg[RDT]): Unit = {
    // println(s"msg to ${destination.id}: $syncMsg")
    val buffer = ArrayMessageBuffer(writeToArray(syncMsg))
    connectionManager.send(destination, buffer): Unit
  }

  protected def sendMultiple(destination: PublicIdentity, syncMsgs: Array[SyncMsg[RDT]]): Unit = {
    if syncMsgs.isEmpty then return
    // println(s"msgs to ${destination.id}: $syncMsgs")
    connectionManager.sendMultiple(
      destination,
      syncMsgs.map(msg => ArrayMessageBuffer(writeToArray(msg)))
    ): Unit
  }

  protected def broadcast(syncMsg: SyncMsg[RDT]): Unit = {
    val buffer = ArrayMessageBuffer(writeToArray(syncMsg))
    connectionManager.broadcast(Array(buffer)): Unit
  }

  // Only updated in message queue thread
  private val peerAddressCache = AtomicReference(Set.empty[(PublicIdentity, (String, Int))])

  // RDT ----------------------
  protected var rdtDeltas: SynchronizedMutableArrayBufferDeltaStore[(RDT, Set[Signature])] =
    SynchronizedMutableArrayBufferDeltaStore()
  @volatile private var partialDeltaStore: Map[Dot, PartialDelta[RDT]] = Map.empty
  @volatile private var peerDots: Map[PublicIdentity, Dots]            = Map.empty

  // Messages -----------------
  // Stores inbound messages
  val msgQueue: LinkedBlockingQueue[(SyncMsg[RDT], PublicIdentity)] = LinkedBlockingQueue()
  private var deltaMessageBacklog                                   = Queue.empty[(RdtDelta[RDT], PublicIdentity)]
  // ACL ----------------------
  // Stores deltas that couldn't be processed because of missing causal dependencies
  private var aclMessageBacklog: Map[Signature, (Set[Signature], SerializedAclOp)] = Map.empty
  private val knownMissingAclOps: AtomicReference[Set[Signature]]                  = AtomicReference(Set.empty)
  @volatile private var peerAclHeads: Map[PublicIdentity, Set[Signature]]          = Map.empty

  // Executed in threads from ConnectionManager, thread safe
  override def receivedMessage(msg: SyncMsg[RDT], fromUser: PublicIdentity): Unit =
    msgQueue.put((msg, fromUser))

  // Executed in thread from ConnectionManager
  override def connectionEstablished(remote: PublicIdentity): Unit = {
    val aclOpGraph = replica.currentOpGraph
    // TODO: It might make sense to tell the other side which document we're talking about. Otherwise they might try to
    //  sync unrelated op graphs
    sendMultiple(
      remote,
      Array(TellKnownAclOps(aclOpGraph.heads, knownMissingAclOps.get()), TellKnownRdtDots(rdtDeltas.dots))
    )
    broadcast(AnnouncePeers(peerAddressCache.get()))
  }

  // Executed in thread from ConnectionManager
  override def connectionShutdown(remote: PublicIdentity): Unit =
    println(s"Disconnected from $remote")

  def connect(peerId: PublicIdentity, host: String, port: Int): Unit = {
    if peerId == localPublicId then return
    connectionManager.connectTo(host, port)
    newPeers(Set((peerId, (host, port))))
  }

  def newPeers(peers: Set[(PublicIdentity, (String, Int))]): Unit =
    receivedMessage(AnnouncePeers(peers), localPublicId)

  def removePeer(user: PublicIdentity): Unit =
      peerAddressCache.updateAndGet(cache => cache.filterNot(_._1 == user))
      connectionManager.disconnect(user)

  def mutateRdt(dot: Dot, delta: RDT): Unit = {
    require(!rdtDeltas.dots.contains(dot))
    val aclOpGraph      = replica.currentOpGraph
    val localWritePerms = aclOpGraph.latestAcl.write.get(localPublicId)
    if localWritePerms.isEmpty
    then
        Console.err.println("Could not mutate RDT: missing write permissions")
        return
    val filteredDelta           = filter.filter(delta, localWritePerms.get)
    val deltaMsg: RdtDelta[RDT] = RdtDelta(dot, filteredDelta, aclOpGraph.heads, aclOpGraph.heads)
    msgQueue.put((deltaMsg, localPublicId))
    broadcastFiltered(aclOpGraph.latestAcl, deltaMsg)
  }

  def broadcastAclDelegation(delegation: SerializedAclOp): Unit = broadcast(AclDelta(delegation))

  @volatile private var stopped = false

  def stop(): Unit = {
    stopped = true
    thread.foreach(_.interrupt())
    connectionManager.shutdown()
  }

  def start(): Unit = {
    require(connectionManager.listenPort.isEmpty) // TODO: Allow restart?
    connectionManager.acceptIncomingConnections()
    peerAddressCache.updateAndGet(cache => cache + (localPublicId -> ("localhost", connectionManager.listenPort.get)))

    val thread = Thread(() =>
      while !stopped do {
        try
            val pollWaitTime = 500 + random.nextInt(1_000) // wait up to .5 to 1.5 seconds for messages to arrive
            processAllMessagesInInbox(pollWaitTime)
            val peers = connectedPeers
            if peers.nonEmpty then notifyPeerAboutLocalState(peers.drop(random.nextInt(peers.size)).head)
        catch {
          case e: InterruptedException =>
        }
      }
    )
    thread.start()
    this.thread = Some(thread)
  }

  protected def receiveMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit = {
    if !replica.currentOpGraph.latestAcl.removed.contains(sender) // Check if user was removed -> drop message
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

  protected def handleMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit = {
    msg match
        case deltaMsg @ RdtDelta(dot, delta, authorAclHeads, senderAclHeads) =>
          if !handlePartialDelta(deltaMsg, sender)
          then deltaMessageBacklog = deltaMessageBacklog.appended((deltaMsg, sender))

        case AclDelta(encodedDelegation) =>
          val aclBefore           = replica.currentAcl
          val localReadPermBefore = aclBefore.read.getOrElse(localPublicId, PermissionTree.empty)
          val missing = replica.applyAclOpIfPossible(encodedDelegation) // throws an exception if invalid signature
          // Track this delta as received (even if it has missing dependencies)
          knownMissingAclOps.updateAndGet(knownMissing => knownMissing - encodedDelegation.signatureAsString)
          if missing.nonEmpty then {
            val knownMissing = knownMissingAclOps.updateAndGet(_ union missing)
            aclMessageBacklog =
              aclMessageBacklog + (encodedDelegation.signatureAsString -> (missing, encodedDelegation))
          } else {
            processAclBacklog(Set(encodedDelegation.signatureAsString))
          }
          // check if local read permissions increased => request potentially missing delta parts
          val aclAfter           = replica.currentAcl
          val localReadPermAfter = aclAfter.read.getOrElse(localPublicId, PermissionTree.empty)
          if localReadPermAfter != localReadPermBefore then {
            // Over approximation because replicas might not have actually written content
            val authorsWithMissingPartialDeltas = aclAfter.write.filter((peer, peerWritePerm) =>
              // Note: "removed" peers aren't considered, since they are removed from aclAfter.write
              // Checks if after is greater than before
              peerWritePerm.intersect(localReadPermBefore) != peerWritePerm.intersect(localReadPermAfter)
              // Note: right - left = delta of permissions
            ).keySet
            val incompleteDots = Dots(rdtDeltas.dots.internal.filter((uid, _) =>
              authorsWithMissingPartialDeltas.contains(PublicIdentity(uid.delegate))
            ))
            val newlyPartialDeltas = rdtDeltas.getAll(incompleteDots).map { case (dot, (delta: RDT, authorAcl)) =>
              val authorWritePermBefore = aclBefore.write(PublicIdentity(dot.place.delegate))
              dot -> PartialDelta(
                delta,
                authorWritePermBefore.intersect(localReadPermBefore),
                authorWritePermBefore.intersect(localReadPermAfter)
              )
            }
            partialDeltaStore = partialDeltaStore ++ newlyPartialDeltas
            rdtDeltas.removed(incompleteDots)
          }

        // heads are placed in local knownMissing if new, locally available ops specified in knownMissing of remote are sent to remote
        case TellKnownAclOps(remoteHeads: Set[Signature], remoteMissing: Set[Signature]) =>
          val opGraph = replica.currentOpGraph
          val msgs    = remoteMissing
            .flatMap { sig => opGraph.ops.get(sig).map(delegation => delegation.serialize(sig)) }
            .map[AclDelta[RDT]](delegation => AclDelta(delegation))
          sendMultiple(sender, msgs.toArray)

          val missingLocally = remoteHeads.filterNot(opGraph.ops.contains)
          if missingLocally.nonEmpty then {
            knownMissingAclOps.updateAndGet(_ union missingLocally): Unit
          }

          if peerAclHeads.getOrElse(sender, Set.empty) != remoteHeads then {
            peerAclHeads = peerAclHeads.updated(sender, remoteHeads)
            // Send our local state at least once to make sure that the other side also knows our local state
            send(sender, TellKnownAclOps(opGraph.heads, knownMissingAclOps.get()))
          }

        case TellKnownRdtDots(remoteRdtDots) =>
          // TODO: avoid possibility of interleaving with acl change
          val aclOpGraph = replica.currentOpGraph

          val rdtDots          = rdtDeltas.dots
          val missingRdtDeltas = rdtDots.subtract(remoteRdtDots)
          val deltas = rdtDeltas.getAll(missingRdtDeltas).map[RdtDelta[RDT]] { case (dot, (delta, authorAcl)) =>
            RdtDelta(dot, delta, authorAcl, aclOpGraph.heads)
          }
          if deltas.nonEmpty then sendFiltered(sender, aclOpGraph.latestAcl, deltas)

          peerDots = peerDots.updatedWith(sender) {
            case Some(old) => Some(old.union(rdtDots))
            case None      => Some(rdtDots)
          }

        case AnnouncePeers(peers) =>
          val peerAddresses = peerAddressCache.getAndUpdate(cache => cache ++ peers)
          val newPeers      = peers.toMap.removedAll(peerAddresses.map(_._1))
          if newPeers.nonEmpty then {
            broadcast(AnnouncePeers(peerAddresses))
          }
          if autoConnect then {
            val connectedPeers = connectionManager.connectedPeers
            newPeers
              .filterNot((publicId, _) => connectedPeers.contains(publicId))
              .foreach { case (user, (host, port)) =>
                connectionManager.connectTo(host, port)
              }
          }
  }

  @tailrec
  private def processAclBacklog(appliedOps: Set[Signature]): Unit = {
    val (ready, remaining) = aclMessageBacklog
      .map { case (sig, (missing, op)) => sig -> (missing -- appliedOps, op) }
      .partition { case (sig, (missing, op)) => missing.isEmpty }
    aclMessageBacklog = remaining
    ready.foreach { case (signature, (_, op)) =>
      val success = replica.applyAclOpIfPossible(op).isEmpty
      assert(success)
    }
    if ready.nonEmpty then processAclBacklog(ready.keySet)
  }

  private def processDeltaBacklog(): Unit = {
    // Process backlogged rdt deltas
    val aclOpGraph                               = replica.currentOpGraph
    val partialDeltaStoreBeforeProcessing        = partialDeltaStore
    val (processableDeltas, unprocessableDeltas) = deltaMessageBacklog.partition((delta, _) =>
      delta.authorAclContext.union(delta.senderAclContext).forall(aclOpGraph.ops.contains)
    )
    deltaMessageBacklog = unprocessableDeltas
    processableDeltas.foreach { (delta, sender) =>
      handlePartialDelta(delta, sender)
    }
  }

  // Returns false if not applicable because local ACL is not superset of author and senders ACL
  protected def handlePartialDelta(unfilteredDelta: RdtDelta[RDT], sender: PublicIdentity): Boolean = {
    unfilteredDelta match
        case RdtDelta(dot, _, authorAclHeads, senderAclHeads) =>
          val aclOpGraph = replica.currentOpGraph
          if !authorAclHeads.union(senderAclHeads).forall(aclOpGraph.ops.contains) then return false

          val authorAcl            = aclOpGraph.reconstruct(authorAclHeads).get
          val senderAcl            = aclOpGraph.reconstruct(senderAclHeads).get
          val effectivePermissions = filter.minimizePermissionTree(
            authorAcl.write(PublicIdentity(dot.place.delegate))
              .intersect(senderAcl.write(sender))
              .intersect(aclOpGraph.latestAcl.read(localPublicId))
          )

          val delta = Filter[RDT].filter(unfilteredDelta.delta, effectivePermissions)

          val existingPartialDelta = partialDeltaStore.get(dot)
          if existingPartialDelta.isEmpty then {
            val requiredPermissions = filter.minimizePermissionTree(
              authorAcl.write(PublicIdentity(dot.place.delegate)).intersect(aclOpGraph.latestAcl.read(localPublicId))
            )
            if requiredPermissions <= senderAcl.write(sender)
            then // Immediately applicable
                rdtDeltas.put(dot, (delta, authorAclHeads))
                replica.receivedDelta(dot, delta)
            else // delta is missing parts
                partialDeltaStore =
                  partialDeltaStore + (dot -> PartialDelta(delta, effectivePermissions, requiredPermissions))
          } else {
            existingPartialDelta.get match
                case PartialDelta(storedDelta, includedParts, requiredPermissions) =>
                  val combinedPermissions = filter.minimizePermissionTree(includedParts.merge(effectivePermissions))
                  if requiredPermissions <= combinedPermissions
                  then // Existing partial delta merged with newly received partial delta is complete
                      val completeDelta = delta.merge(storedDelta)
                      rdtDeltas.put(dot, (completeDelta, authorAclHeads))
                      replica.receivedDelta(dot, completeDelta)
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

  protected def sendFiltered(receiver: PublicIdentity, acl: BftAcl, deltas: Array[RdtDelta[RDT]]): Unit = {
    val permissions = acl.read(receiver).intersect(acl.write(localPublicId))
    sendMultiple(
      receiver,
      deltas.map(delta => delta.copy(delta = filter.filter(delta.delta, permissions)))
    )
  }

  protected def broadcastFiltered(acl: BftAcl, delta: RdtDelta[RDT]): Unit = {
    connectionManager.connectedPeers.foreach { receiver =>
      val permissions = acl.write.getOrElse(localPublicId, PermissionTree.empty).intersect(
        acl.read.getOrElse(receiver, PermissionTree.empty)
      )
      if !permissions.isEmpty then {
        send(receiver, delta.copy(delta = filter.filter(delta.delta, permissions)))
      } else {
        send(receiver, delta.copy(delta = rdtBottom.empty))
      }
    }
  }

  private def requestPartialDeltas(): Unit = {
    val aclOpGraph    = replica.currentOpGraph
    val partialDeltas = partialDeltaStore
    val requiredPerms =
      if partialDeltas.size < 100
      then // Only need to reconstruct what is missing
          partialDeltas.values.map(_.requiredPermissions).reduce(Lattice.merge)
      else // Request everything that is readable
          aclOpGraph.latestAcl.read.getOrElse(localPublicId, PermissionTree.empty)

    // TODO: Could be optimized to reduce number of replicas to contact and deltas to request at the cost of complexity
    // - If a replica won't provide progress, don't request specific dot
    //    - progress can be used for this: !(goal ⋂ writer_permission <= progress)
    // - Provide existing progress to PartialReplicationPeerSubsetSolver
    // - Provide union of requiredPermissions of partial deltas instead of local read permission
    val (replicasToRequestFrom, _) =
      PartialReplicationPeerSubsetSolver.randomSubsetThatAllowsReconstruction(
        aclOpGraph.latestAcl.write,
        requiredPerms
      )
    replicasToRequestFrom.foreach { remote =>
      send(remote, TellKnownRdtDots[RDT](rdtDeltas.dots))
    }
  }

  protected[bft] def notifyPeerAboutLocalState(peer: PublicIdentity): Unit = {
    val aclOpGraph = replica.currentOpGraph
    if partialDeltaStore.nonEmpty then {
      requestPartialDeltas()
      if peerAclHeads.getOrElse(peer, Set.empty) != aclOpGraph.heads then
          send(peer, TellKnownAclOps(aclOpGraph.heads, knownMissingAclOps.get()))
    } else {
      // Request from a random peer that has more deltas than this replica
      val replicasWithNewerDots = peerDots.filter((id, dots) => !rdtDeltas.dots.contains(dots))
      if replicasWithNewerDots.nonEmpty then
          peerDots.drop(random.nextInt(replicasWithNewerDots.size)).headOption.foreach {
            case (peerWithMoreDots, _) => send(peerWithMoreDots, TellKnownRdtDots(rdtDeltas.dots))
          }

      // Check RDT sync state with peer
      send(peer, TellKnownRdtDots(rdtDeltas.dots))

      // Check ACL sync state with peer unless assumed to be equal
      if peerAclHeads.getOrElse(peer, Set.empty) != aclOpGraph.heads then
          send(peer, TellKnownAclOps(aclOpGraph.heads, knownMissingAclOps.get()))
    }
  }

  protected[bft] def processAllMessagesInInbox(incomingMessagePollTimeoutMillis: Int): Unit = {
    var nextMessage = Option(msgQueue.poll(incomingMessagePollTimeoutMillis, TimeUnit.MILLISECONDS))
    while nextMessage.nonEmpty do {
      val (msg, sender) = nextMessage.get
      receiveMessage(msg, sender)
      nextMessage = Option(msgQueue.poll())
    }
  }
}

object BftFilteringAntiEntropy {
  enum SyncMsg[RDT]:
      case RdtDelta(dot: Dot, delta: RDT, authorAclContext: Set[Signature], senderAclContext: Set[Signature])
      case AclDelta(serializedAclOp: SerializedAclOp)
      case AnnouncePeers(peers: Set[(PublicIdentity, (String, Int))])
      case TellKnownRdtDots(rdtDots: Dots)
      // TODO: Improve anti entropy of ACL with bloom filter/riblt based sync
      case TellKnownAclOps(aclVersion: Set[Signature], knownMissing: Set[Signature])

  object SyncMsg {
    given codec[RDT](using JsonValueCodec[RDT]): JsonValueCodec[SyncMsg[RDT]] = {
      import lofi_acl.sync.JsoniterCodecs.uidKeyCodec
      JsonCodecMaker.make
    }
  }

}
