package ex2024travel.lofi_acl.sync.bft.eval

import channels.{ArrayMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import ex2024travel.lofi_acl.sync.*
import ex2024travel.lofi_acl.sync.bft.BftFilteringAntiEntropy.SyncMsg
import ex2024travel.lofi_acl.sync.bft.BftFilteringAntiEntropy.SyncMsg.{RdtDelta, TellKnownRdtDots, codec}
import ex2024travel.lofi_acl.sync.bft.{BftAcl, BftFilteringAntiEntropy, ReplicaWithBftAcl, SerializedAclOp}
import rdts.base.{Bottom, Lattice}
import rdts.filters.Filter
import rdts.time.Dot

class NonFilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    aclRoot: SerializedAclOp,
    replica: ReplicaWithBftAcl[RDT],
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    autoConnect: Boolean = true
)(using rdtCodec: JsonValueCodec[RDT], filter: Filter[RDT], rdtLattice: Lattice[RDT], rdtBottom: Bottom[RDT])
    extends BftFilteringAntiEntropy[RDT](localIdentity, aclRoot, replica, connectionManagerProvider, autoConnect) {

  override protected def sendFiltered(
      receiver: PublicIdentity,
      acl: BftAcl,
      deltas: Array[RdtDelta[RDT]]
  ): Unit = {
    // Don't apply filter
    connectionManager.sendMultiple(
      receiver,
      deltas.map(msg => ArrayMessageBuffer(writeToArray(msg)(using codec)))
    ): Unit
  }

  override protected def broadcastFiltered(acl: BftAcl, delta: SyncMsg.RdtDelta[RDT]): Unit =
    connectionManager.broadcast(Array(ArrayMessageBuffer(writeToArray(delta)(using codec))))

  override protected[bft] def notifyPeerAboutLocalState(peer: PublicIdentity): Unit =
    send(peer, TellKnownRdtDots(rdtDeltas.dots))

  override def mutateRdt(dot: Dot, delta: RDT): Unit = {
    val deltaMsg = RdtDelta(dot, delta, Set.empty, Set.empty)
    msgQueue.put((deltaMsg, localPublicId))
    broadcast(deltaMsg)
  }

  override protected def handleMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit = {
    msg match {
      case SyncMsg.RdtDelta(dot, delta, _, _)                => super.handleMessage(msg, sender)
      case SyncMsg.TellKnownRdtDots(rdtDots)                 => super.handleMessage(msg, sender)
      case SyncMsg.AclDelta(serializedAclOp)                 =>
      case SyncMsg.TellKnownAclOps(aclVersion, knownMissing) =>
      case SyncMsg.AnnouncePeers(peers)                      => super.handleMessage(msg, sender)
    }
    super.handleMessage(msg, sender)
  }

  override protected def receiveMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit =
    handleMessage(msg, sender)

  override protected def handlePartialDelta(delta: SyncMsg.RdtDelta[RDT], sender: PublicIdentity): Boolean =
      rdtDeltas.put(delta.dot, (delta.delta, Set.empty))
      replica.receivedDelta(delta.dot, delta.delta)
      true
}
