package probench.data

import probench.data.RequestResponseQueue.Req
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.protocols.Participants
import rdts.protocols.paper.{MultiPaxos, Vote, Voting}
import replication.ProtocolMessage
import replication.ProtocolMessage.Payload

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ConnInformation = HeartbeatQuorum
type ClusterState    = MultiPaxos[ClientCommWrite.WriteReq]

enum ClientCommRead:
  case ReadReq(id: Uid, kvOperation: KVOperation.Read[String,String])
  case ReadRes(id: Uid, value: String)
enum ClientCommWrite:
  case WriteReq(id: Uid, kvOperation: KVOperation.Write[String,String])
  case WriteRes(id: Uid, value: String)
  
object ClientComm {
  given l1: Lattice[Payload[ClientCommWrite]] =
    given Lattice[Int] = Lattice.fromOrdering
    given Lattice[ClientCommWrite] = Lattice.assertEquals

    Lattice.derived

  given l2: Lattice[Payload[ClientCommRead]] =
    given Lattice[Int] = Lattice.fromOrdering
    given Lattice[ClientCommRead] = Lattice.assertEquals

    Lattice.derived
}

object Codecs {
  import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
  import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

  // codecs
  given JsonValueCodec[ClusterState] =
  JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given clusterCodec: JsonValueCodec[ProtocolMessage[ClusterState]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given JsonValueCodec[ClientCommRead] = JsonCodecMaker.make
  given JsonValueCodec[ClientCommWrite] = JsonCodecMaker.make
  given JsonValueCodec[ConnInformation] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}

case class Heartbeat(supposedLeader: Option[Uid], senderTimestamp: Long, receiverTimestamp: Option[Long] = None)
case class HeartbeatQuorum(heartbeats: Map[Uid, LastWriterWins[Heartbeat]] =
  Map.empty[Uid, LastWriterWins[Heartbeat]]) {
  private def currentVotes(timeoutThreshold: Long, currentTime: Long)(using LocalUid): Map[Uid, Option[Uid]] = {
    // count votes that were received in the threshold window and current local votes
    heartbeats
      .collect {
        case (uid, LastWriterWins(_, Heartbeat(leader, senderT, Some(receiverT))))
            if receiverT >= (currentTime - timeoutThreshold) =>
          (uid, leader)
        case (uid, LastWriterWins(_, Heartbeat(leader, senderT, _)))
            if senderT >= (currentTime - timeoutThreshold) && uid == replicaId =>
          (uid, leader)
      }
  }

  def alivePeers(timeoutThreshold: Long, currentTime: Long)(using LocalUid): Set[Uid] = {
    currentVotes(timeoutThreshold, currentTime)
      .map((uid, _) => uid)
      .toSet
  }

  def hasQuorum(timeoutThreshold: Long, currentTime: Long)(using LocalUid, Participants): Boolean = {
    Voting(
      currentVotes(timeoutThreshold, currentTime).collect {
        case (id, Some(leader)) => Vote(value = leader, voter = id)
      }.toSet
    ).result.contains(replicaId)
  }
}

object HeartbeatQuorum:
    given Lattice[HeartbeatQuorum] = Lattice.derived

case class KVState(
    requests: RequestResponseQueue[KVOperation[String, String], String] = RequestResponseQueue.empty,
    clusterState: MultiPaxos[Req[KVOperation[String, String]]] = MultiPaxos.empty
):
    def upkeep(using LocalUid, Participants): KVState =
      KVState(clusterState = clusterState.upkeep)

object KVState:
    given Lattice[KVState] =
      Lattice.derived

    def empty: KVState = KVState()
