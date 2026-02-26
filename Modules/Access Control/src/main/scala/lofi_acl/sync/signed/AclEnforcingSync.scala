package lofi_acl.sync.signed

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.*
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.sync.JsoniterCodecs.syncMsgCodec
import lofi_acl.sync.signed.AclEnforcingSync.SyncMsg.MyPeersAre
import lofi_acl.sync.signed.AclEnforcingSync.{SyncMsg, encoder}
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, JsoniterCodecs, MessageReceiver}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dots

import java.util.concurrent.Executors

class AclEnforcingSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    aclGenesis: BftDelta[Acl],
    onRdtChanged: State => Unit
) {
  private val messageHandlerExecutor = Executors.newSingleThreadExecutor() // Executes the message handling logic
  private val connectionManager: ConnectionManager = {
    val msgReceiver = new MessageReceiver[MessageBuffer] {
      override def receivedMessage(msg: MessageBuffer, fromUser: PublicIdentity): Unit =
        messageHandlerExecutor.execute(() => handleMessage(readFromArray(msg.asArray), fromUser))

      override def connectionEstablished(newRemote: PublicIdentity): Unit =
        messageHandlerExecutor.execute(() =>
          val msg = ArrayMessageBuffer(writeToArray(MyPeersAre(connectionManager.peerAddresses.toSeq)))
          connectionManager.connectedPeers.foreach { remote =>
            connectionManager.send(remote, msg)
          }
        )
    }
    connectionManagerProvider(localIdentity, msgReceiver)
  }
  private val comm                                 = ConnectionManagerCommunicator(connectionManager)

  private val aclAntiEntropy = AclAntiEntropy(localIdentity, aclGenesis, onAclChange, comm)
  private val rdtAntiEntropy: FilteredRdtAntiEntropy[State] =
    FilteredRdtAntiEntropy[State](localIdentity, onRdtChanged, comm, aclAntiEntropy)

  private def onAclChange(delta: Acl): Unit = if rdtAntiEntropy != null then rdtAntiEntropy.onAclChanged(delta)

  def handleMessage(msg: SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.DataDeltas(deltas, filtered, remoteAcl) => aclAntiEntropy.updatePeerAclKnowledge(remoteAcl, remote)
    case SyncMsg.AclDeltas(deltas)                       => aclAntiEntropy.receiveDeltas(deltas, remote)
    case SyncMsg.MyPeersAre(peers)                       =>
      val established = connectionManager.connectedPeers
      peers.filter((id, _) => established(id)).foreach { case (_, (host, port)) =>
        connectionManager.connectTo(host, port)
      }
    case SyncMsg.MyRdtVersionIs(remoteDataDeltas) => rdtAntiEntropy.updatePeerDeltaKnowledge(remoteDataDeltas, remote)
    case SyncMsg.MyAclVersionIs(remoteAclHeads)   => aclAntiEntropy.updatePeerAclKnowledge(remoteAclHeads, remote)
    case SyncMsg.SendMe(missingRdtDeltas, missingAclDeltas) =>
      if missingRdtDeltas.nonEmpty then rdtAntiEntropy.respondToDeltaRequest(missingRdtDeltas, remote)
      if missingAclDeltas.nonEmpty then aclAntiEntropy.respondToDeltaRequest(missingAclDeltas, remote)
  }

  def connect(host: String, port: Int): Unit = connectionManager.connectTo(host, port)

  def currentState: State = rdtAntiEntropy.currentState._2

  def aclRootOp: BftDelta[Acl] = {
    val hashDag = aclAntiEntropy.currentHashDag
    hashDag.deltas(hashDag.root)
  }

  def currentAcl: Acl = aclAntiEntropy.currentAcl._2

  def delegatePermission(read: Map[PublicIdentity, PermissionTree], write: Map[PublicIdentity, PermissionTree]): Unit =
    aclAntiEntropy.mutate(Acl(read, write, Set.empty, Set.empty))

  def mutate(mutator: State => State): Unit =
    rdtAntiEntropy.localMutation(mutator)

  def listenPort: Option[Int] = connectionManager.listenPort

  def start(): Unit = {
    connectionManager.acceptIncomingConnections()
  }

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
}
