package probench.data

import probench.data.RequestResponseQueue.Req
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.protocols.Participants
import rdts.protocols.paper.{MultiPaxos, PipePaxos, Vote, Voting}

enum KVOperation[Key, Value] {
  def key: Key

  case Read(key: Key)
  case Write(key: Key, value: Value)
}

type ConnInformation = Map[LocalUid, LastWriterWins[Long]]
type ClusterState    = MultiPaxos[Req[KVOperation[String, String]]]
type ClientState     = RequestResponseQueue[KVOperation[String, String], String]

case class Heartbeat(supposedLeader: Uid, timestamp: Long)
case class HeartbeatQuorum(heartbeats: Map[Uid, LastWriterWins[Heartbeat]]) {
  private def currentVotes(timeoutThreshold: Long): Map[Uid, Uid] =
    heartbeats
      .filter((_, lww) => lww.value.timestamp >= (System.currentTimeMillis() - timeoutThreshold))
      .map((uid, lww) => (uid, lww.value.supposedLeader))

  def alivePeers(timeoutThreshold: Long): Set[Uid] = {
    currentVotes(timeoutThreshold)
      .map((uid, _) => uid)
      .toSet
  }

  def hasQuorum(timeoutThreshold: Long)(using LocalUid, Participants): Boolean = {
    Voting(
      currentVotes(timeoutThreshold)
        .map((id, leader) => Vote(value = leader, voter = id))
        .toSet
    ).result.contains(replicaId)
  }
}

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
