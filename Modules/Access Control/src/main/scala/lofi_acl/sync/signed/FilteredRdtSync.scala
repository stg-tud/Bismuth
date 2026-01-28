package lofi_acl.sync.signed

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.*
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.sync.signed.FilteredRdtSync.SyncMsg.MyPeersAre
import lofi_acl.sync.signed.FilteredRdtSync.{SyncMsg, encoder}
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, JsoniterCodecs, MessageReceiver}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.Dots

import java.util.concurrent.LinkedBlockingQueue

class FilteredRdtSync[State: {JsonValueCodec, Bottom, Decompose, Lattice, Filter}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    initialAclHashDag: HashDag[BftDelta[Acl], Acl]
) extends MessageReceiver[SyncMsg[State]] {
  private val msgQueue: LinkedBlockingQueue[(SyncMsg[State], PublicIdentity)] = LinkedBlockingQueue()
  private val comm: Communication[SyncMsg[State]]                             = ???

  private val aclAntiEntropy = AclAntiEntropy(localIdentity, initialAclHashDag)
  private val rdtAntiEntropy = FilteredRdtAntiEntropy[State](localIdentity)

  override def receivedMessage(msg: SyncMsg[State], remote: PublicIdentity): Unit = msg match {
    case SyncMsg.DataDeltas(deltas, filtered, remoteAcl) =>
      aclAntiEntropy.updatePeerAclKnowledge(remoteAcl, remote)
    case SyncMsg.AclDeltas(deltas)                                => aclAntiEntropy.receiveDeltas(deltas, remote)
    case SyncMsg.MyPeersAre(peers)                                => ???
    case SyncMsg.MyLocalStateIs(remoteDataDeltas, remoteAclHeads) =>
      rdtAntiEntropy.updatePeerDeltaKnowledge(remoteDataDeltas, remote)
      aclAntiEntropy.updatePeerAclKnowledge(remoteAclHeads, remote)
    case SyncMsg.SendMe(missingRdtDeltas, missingAclDeltas) =>
      rdtAntiEntropy.respondToDeltaRequest(missingRdtDeltas, remote)
      aclAntiEntropy.respondToDeltaRequest(missingAclDeltas, remote)
  }
}

trait Communication[Msg] {
  def send(to: PublicIdentity, msg: Msg): Unit
  def sendMultiple(to: PublicIdentity, msg: Msg): Unit
  def handlePeerGossip(peers: Set[(PublicIdentity, (String, Int))]): Unit
}

object FilteredRdtSync {
  enum SyncMsg[State]:
      case DataDeltas(deltas: Seq[SignedDelta[State]], filtered: Dots, acl: Set[Hash])
      case AclDeltas(delta: Seq[BftDelta[Acl]])
      case MyPeersAre(peers: Seq[(PublicIdentity, (String, Int))])
      case MyLocalStateIs(dataDeltas: Dots, aclHeads: Set[Hash])
      case SendMe(dataDeltas: Dots, aclDeltas: Set[Hash])

  given encoder[State: JsonValueCodec]: Encoder[SignedDelta[State]] =
    Encoder.fromJsoniter(using JsoniterCodecs.filterableSignedDeltaCodec)
}
