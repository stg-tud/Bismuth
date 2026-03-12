package lofi_acl.sync.anti_entropy

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.JsoniterCodecs.syncMsgCodec
import lofi_acl.bft.*
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.sync.anti_entropy.AclEnforcingSync.SyncMsg.{MyAclVersionIs, MyPeersAre, MyRdtVersionIs}
import lofi_acl.sync.anti_entropy.AclEnforcingSync.{SyncMsg, encoder}
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import lofi_acl.{Debug, JsoniterCodecs}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dots

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

class AclEnforcingSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id, receiver),
    aclGenesis: BftDelta[Acl],
    onRdtChanged: State => Unit
) {
  private val messageHandlerExecutor = Executors.newSingleThreadExecutor() // Executes the message handling logic
  protected val connectionManager: ConnectionManager = {
    val msgReceiver = new MessageReceiver[MessageBuffer] {
      override def receivedMessage(msgBuf: MessageBuffer, fromUser: PublicIdentity): Unit =
        messageHandlerExecutor.execute(() =>
            val msg: SyncMsg[State] = readFromArray(msgBuf.asArray)
            Debug.received(msg, fromUser)
            handleMessage(msg, fromUser)
        )

      override def connectionEstablished(newRemote: PublicIdentity): Unit =
        messageHandlerExecutor.execute(() => onConnectionEstablished(newRemote))
    }
    connectionManagerProvider(localIdentity, msgReceiver)
  }

  protected val comm                             = ConnectionManagerCommunicator(connectionManager)
  protected val (aclAntiEntropy, rdtAntiEntropy) = instantiateAntiEntropy()

  protected def instantiateAntiEntropy(): (AclAntiEntropy, FilteredRdtAntiEntropy[State]) = {
    val aclAntiEntropy                                = AclAntiEntropy(localIdentity, aclGenesis, onAclChange, comm)
    val rdtAntiEntropy: FilteredRdtAntiEntropy[State] =
      FilteredRdtAntiEntropy[State](localIdentity, onRdtChanged, comm, aclAntiEntropy)

    (aclAntiEntropy, rdtAntiEntropy)
  }

  private val remoteAddressCache: AtomicReference[Map[PublicIdentity, Set[(String, Int)]]] = AtomicReference(Map.empty)

  protected def onAclChange(delta: Acl): Unit = if rdtAntiEntropy != null then rdtAntiEntropy.onAclChanged(delta)

  protected def onConnectionEstablished(newRemote: PublicIdentity): Unit = {
    // Notify remote about local ACL state
    val aclVersionMsg = ArrayMessageBuffer(writeToArray(MyAclVersionIs(aclAntiEntropy.currentAcl._1)))
    // Notify remote about local RDT state
    val rdtVersionMsg = ArrayMessageBuffer(writeToArray(MyRdtVersionIs(rdtAntiEntropy.currentState._1)))
    // Only tell new peer about our peers (instead of everyone)
    val peersMsg = ArrayMessageBuffer(writeToArray(MyPeersAre(
      // Tell remote my listen address (important, if we have established a connection)
      (localIdentity.getPublic -> connectionManager.listenAddress.get) +:
      // Tell remote about other replicas' addresses
      remoteAddressCache.get().flatMap((id, addrs) => addrs.map(addr => id -> addr)).toSeq
    )))

    connectionManager.sendMultiple(newRemote, Array(aclVersionMsg, rdtVersionMsg))
    connectionManager.broadcast(Array(peersMsg))
  }

  def handleMessage(msg: SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.DataDeltas(deltas, filtered, remoteAcl) =>
      aclAntiEntropy.updatePeerAclKnowledge(remoteAcl, remote)
      rdtAntiEntropy.receiveDeltas(deltas, filtered, remoteAcl, remote)
    case SyncMsg.AclDeltas(deltas) => aclAntiEntropy.receiveDeltas(deltas, remote)
    case SyncMsg.MyPeersAre(peers) =>
      val peersAsMap   = peers.groupMapReduce(_._1)(kv => Set(kv._2))((l, r) => l ++ r)
      val oldAddresses = remoteAddressCache.getAndUpdate(oldMap => Lattice.merge(oldMap, peersAsMap))

      val established = connectionManager.connectedPeers
      val newPeers    = peers.filterNot((id, _) => established(id) || localIdentity.getPublic == id)
      newPeers.foreach { case (id, (host, port)) => connect(id, host, port) }

      if oldAddresses != remoteAddressCache.get() then
          val peersMsg = ArrayMessageBuffer(writeToArray(MyPeersAre(
            // Tell remote my listen address (important, if we have established a connection)
            (localIdentity.getPublic -> connectionManager.listenAddress.get) +:
            // Tell remote about other replicas' addresses
            remoteAddressCache.get().flatMap((id, addrs) => addrs.map(addr => id -> addr)).toSeq
          )))
          connectionManager.broadcast(Array(peersMsg))

      Debug.log(s"Learned: ${newPeers.map((id, addr) => Debug.shorten(id) + " " + addr)}")
    case SyncMsg.MyRdtVersionIs(remoteDataDeltas) => rdtAntiEntropy.updatePeerDeltaKnowledge(remoteDataDeltas, remote)
    case SyncMsg.MyAclVersionIs(remoteAclHeads)   => aclAntiEntropy.updatePeerAclKnowledge(remoteAclHeads, remote)
    case SyncMsg.SendMe(missingRdtDeltas, missingAclDeltas) =>
      if missingRdtDeltas.nonEmpty then rdtAntiEntropy.respondToDeltaRequest(missingRdtDeltas, remote)
      if missingAclDeltas.nonEmpty then aclAntiEntropy.respondToDeltaRequest(missingAclDeltas, remote)
  }

  def connect(remoteId: PublicIdentity, host: String, port: Int): Unit = {
    Debug.log(s"Attempting connection to $remoteId ($host:$port)")
    connectionManager.connectTo(host, port)
    remoteAddressCache.updateAndGet(oldMap =>
      oldMap.updatedWith(remoteId) {
        case Some(existingAddresses) => Some(existingAddresses + (host -> port))
        case None                    => Some(Set(host -> port))
      }
    ): Unit
  }

  def aclRootOp: BftDelta[Acl] = {
    val hashDag = aclAntiEntropy.currentHashDag
    hashDag.deltas(hashDag.root)
  }

  def stateVersion: Dots = rdtAntiEntropy.currentState._1

  def currentState: State = rdtAntiEntropy.currentState._2

  def aclVersion: Set[Hash] = aclAntiEntropy.currentAcl._1

  def currentAcl: Acl = aclAntiEntropy.currentAcl._2

  def connectedPeers: Set[PublicIdentity] = connectionManager.connectedPeers

  def delegatePermission(
      read: Map[PublicIdentity, PermissionTree] = Map.empty,
      write: Map[PublicIdentity, PermissionTree] = Map.empty
  ): Unit =
    aclAntiEntropy.mutate(Acl(read, write, Set.empty, Set.empty))

  def delegatePermission(acl: Acl): Unit = {
    require(acl.removed.isEmpty) // Unsupported as of now
    aclAntiEntropy.mutate(acl)
  }

  def mutate(mutator: State => State): Unit =
    rdtAntiEntropy.localMutation(mutator)

  def listenAddress: Option[(String, Int)] = connectionManager.listenAddress

  def start(): Unit =
    connectionManager.acceptIncomingConnections()

  def stop(): Unit = {
    messageHandlerExecutor.shutdown()
    connectionManager.shutdown()
  }
}

object AclEnforcingSync {
  enum SyncMsg[State]:
      case DataDeltas(deltas: Seq[SignedDelta[State]], filtered: Dots, acl: Set[Hash])
      case AclDeltas(delta: Seq[BftDelta[Acl]])
      case MyPeersAre(peers: Seq[(PublicIdentity, (String, Int))])
      case MyAclVersionIs(aclHeads: Set[Hash])
      case MyRdtVersionIs(dots: Dots)
      case SendMe(dataDeltas: Dots, aclDeltas: Set[Hash])

  given encoder[State: JsonValueCodec]: Encoder[SignedDelta[State]] =
    Encoder.fromJsoniter(using JsoniterCodecs.filterableSignedDeltaCodec)

  given Lattice[Map[PublicIdentity, Set[(String, Int)]]] = Lattice.mapLattice(using Lattice.setLattice)
}
