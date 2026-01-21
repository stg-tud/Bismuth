package lofi_acl.sync.signed

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.*
import lofi_acl.sync.signed.FilteredRdtSync.SyncMsg
import lofi_acl.sync.signed.FilteredRdtSync.SyncMsg.PeerGossip
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import rdts.base.Bottom
import rdts.time.{Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue

class FilteredRdtSync[State: {JsonValueCodec, Bottom}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    initialAclHashDag: HashDag[SignedDelta[Acl], Acl]
) extends MessageReceiver[SyncMsg[State]] {
  val msgQueue: LinkedBlockingQueue[(SyncMsg[State], PublicIdentity)] = LinkedBlockingQueue()
  val comm: Communication[SyncMsg[State]]                             = ???
  val aclAntiEntropy                                                  = AclAntiEntropy(localIdentity, initialAclHashDag)

  override def receivedMessage(msg: SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.DataDelta(delta)                                 => ???
    case SyncMsg.AclDeltas(deltas)                                => aclAntiEntropy.receiveDeltas(deltas, remote)
    case SyncMsg.PeerGossip(peers)                                => ???
    case SyncMsg.MyLocalStateIs(remoteDataDeltas, remoteAclHeads) =>
      ??? // dataDeltas
      aclAntiEntropy.updatePeerAclKnowledge(remoteAclHeads, remote)
    case SyncMsg.PleaseSendMe(missingDataDeltas, missingAclDeltas) =>
      ??? // dataDeltas
      aclAntiEntropy.respondToDeltaRequest(missingAclDeltas, remote)
  }
}

trait Communication[Msg: JsonValueCodec] {
  def send(to: PublicIdentity, msg: Msg): Unit
  def sendMultiple(to: PublicIdentity, msg: Msg): Unit
  def handlePeerGossip(peers: Set[(PublicIdentity, (String, Int))]): Unit
}

object FilteredRdtSync {
  enum SyncMsg[State]:
      case DataDelta(delta: FilterableSignedDelta[State])
      case AclDeltas(delta: Vector[SignedDelta[Acl]])
      case PeerGossip(peers: Set[(PublicIdentity, (String, Int))])
      case MyLocalStateIs(dataDeltas: Dots, aclHeads: Set[Hash])
      case PleaseSendMe(dataDeltas: Dots, aclDeltas: Set[Hash])

  given msgCodec[State: JsonValueCodec]: JsonValueCodec[SyncMsg[State]] = {
    import lofi_acl.sync.JsoniterCodecs.given
    import replication.JsoniterCodecs.given
    given JsonValueCodec[FilterableSignedDelta[State]] = JsonCodecMaker.make
    JsonCodecMaker.make
  }
}
