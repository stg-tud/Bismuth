package replication.sync

import channels.connection.ByteBufferMessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.PublicIdentity
import rdts.base.Lattice
import replication.authz.AntiEntropy
import replication.sync.FullMeshControlPlane.{Peers, mapOfPeerAddressesLattice}

import java.nio.ByteBuffer

class FullMeshControlPlane(
    publicIdentity: PublicIdentity,
    connectionManager: ConnectionManager
) extends MessageReceiver[ByteBuffer] {

  private var learnedAddresses: Map[PublicIdentity, Set[(String, Int)]] =
    connectionManager.listenAddress.map(addr => Map(publicIdentity -> Set(addr))).getOrElse(Map.empty)

  override def receivedMessage(msg: ByteBuffer, fromUser: PublicIdentity): Unit = {
    require(msg.get() == AntiEntropy.CONTROL_PLANE_MSG_TAG, "Not a control plane message")
    val peers = readFromByteBuffer[Peers](msg)
    learnedAddresses = mapOfPeerAddressesLattice.merge(learnedAddresses, peers.peers)
  }

  override def connectionEstablished(publicIdentity: PublicIdentity): Unit = {
    val peers         = Peers(learnedAddresses)
    val encodedPeers  = writeToArray(peers)
    val msgByteBuffer = ByteBuffer.allocate(encodedPeers.length + 1)
    msgByteBuffer.put(AntiEntropy.CONTROL_PLANE_MSG_TAG)
    msgByteBuffer.put(encodedPeers)
    msgByteBuffer.reset()
    connectionManager.send(publicIdentity, ByteBufferMessageBuffer(msgByteBuffer))
  }
}

object FullMeshControlPlane {
  case class Peers(peers: Map[PublicIdentity, Set[(String, Int)]])

  import replication.JsoniterCodecsJvm.given
  given codec: JsonValueCodec[Peers]                                                = JsonCodecMaker.make
  given mapOfPeerAddressesLattice: Lattice[Map[PublicIdentity, Set[(String, Int)]]] = Lattice.mapLattice
}
