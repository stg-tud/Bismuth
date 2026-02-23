package probench

import channels.ConcurrencyHelper
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import probench.Codecs.given
import probench.data.*
import probench.data.RequestResponseQueue.Req
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.protocols.Participants
import rdts.protocols.paper.{MultiPaxos, MultipaxosPhase}
import rdts.time.Time
import replication.DeltaStorage.Type.*
import replication.ProtocolMessage.Payload
import replication.{DeltaDissemination, DeltaStorage}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

sealed trait ClientProtocol
object ClientProtocol {
  case class ClientRequest(req: Req[KVOperation[String, String]])                   extends ClientProtocol
  case class ClusterAnswer(req: Req[KVOperation[String, String]], decision: String) extends ClientProtocol

  given JsonValueCodec[ClientProtocol] = JsonCodecMaker.make
}

class KeyValueReplica(
    val uid: Uid,
    val votingReplicas: Set[Uid],
    offloadSending: Boolean = true,
    offloadReplica: Boolean = true,
    deltaStorageType: DeltaStorage.Type = KeepAll,
    timeoutThreshold: Long = 1000,
    commitReads: Boolean = false
) {

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  val sendingActor: ExecutionContext = ConcurrencyHelper.makeExecutionContext(offloadSending)
  val replicaActor: ExecutionContext = ConcurrencyHelper.makeExecutionContext(offloadReplica)

  given Participants(votingReplicas)
  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef = new {}

  private val kvCache = mutable.HashMap[String, String]()

  // ============== CLUSTER ==============

  val cluster: Cluster = new Cluster()
  val client: Client   = new Client()
  val connInf: ConnInf = new ConnInf()

  replicaActor.execute { () =>
    cluster.maybeLeaderElection(votingReplicas)
  }

  class Cluster {
    @volatile var state: ClusterState = MultiPaxos.empty

    given Lattice[Payload[ClusterState]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    val dataManager: DeltaDissemination[ClusterState] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncoming(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => state)
    )

    def handleIncoming(delta: ClusterState): Unit = currentStateLock.synchronized {
      log(s"handling incoming $delta")
      val (old, changed) = currentStateLock.synchronized {
        val old = state
        state = state `merge` delta
        (old, state)
      }
      if old != changed then {
        val upkept = changed.upkeep
        if state.subsumes(upkept)
        then log("no changes")
        else log("upkeep")
        assert(changed == state)
        // else log(s"upkept: ${pprint(upkept)}")
        val newState = publish(upkept)
        maybeAnswerClientFromLog(old.nextDecisionRound)
        // try to propose a new value in case voting is decided
        maybeProposeNewValue(client.state)
      }
    }

    def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
      if delta `inflates` state then {
        log("publishing")
        val oldstate = state
        state = state.merge(delta)
        dataManager.applyDelta(delta)
        maybeAnswerClientFromLog(oldstate.nextDecisionRound)
      } else {
        log("skip")
      }

      state
    }

    def forceUpkeep(): ClusterState = currentStateLock.synchronized {
      publish(state.upkeep)
    }

    def needsUpkeep(): Boolean = currentStateLock.synchronized {
      val curState = state
      val delta    = curState.upkeep
      curState != (curState `merge` delta)
    }

    /** propose myself as leader if I have the lowest id */
    def maybeLeaderElection(peers: Set[Uid]): Unit = {
      peers.minOption match
          case Some(id) if id == uid =>
            log(s"Proposing election of $uid")
            publish(state.startLeaderElection): Unit
          case _ => ()
    }

    def maybeProposeNewValue(client: ClientState)(using LocalUid): Unit = {
      // check if we are the leader and ready to handle a request
      if state.leader.contains(replicaId) && state.phase == MultipaxosPhase.Idle then
          // ready to propose value
          val requestToAnswer =
            if commitReads then client.firstUnansweredRequest
            else
                client.requestsSorted.collectFirst {
                  case r @ Req(KVOperation.Write(_, _), _) => r
                }
          requestToAnswer match
              case Some(req) =>
                log(s"Proposing new value $req.")
                val _ = publish(state.proposeIfLeader(req))
              case None =>
                log("I am the leader but request queue is empty.")
    }

    private def performOp(op: KVOperation[String, String]): String =
      op match {
        case KVOperation.Read(key) =>
          kvCache.synchronized {
            kvCache.getOrElse(key, s"Key '$key' has not been written to!")
          }
        case KVOperation.Write(key, value) =>
          kvCache.synchronized {
            kvCache.put(key, value)
          }
          s"$key=$value; OK"
      }

    def maybeAnswerClientFromCache(clientState: ClientState): Unit = {
      // check if we are the leader and have a heartbeat quorum
      if !commitReads && state.leader.contains(replicaId) && connInf.state.hasQuorum(
            timeoutThreshold,
            System.currentTimeMillis()
          )
      then {
        // ready to propose value
        val responses = clientState.requests.queryAllEntries.collect {
          case req @ Req(k @ KVOperation.Read(key), _) =>
            clientState.respond(req, performOp(k))
        }

        if responses.size > 0 then {
          log("answering unanswered read requests from cache")
          val accumulatedResponses = responses.fold(RequestResponseQueue.empty)((acc, r) => acc.merge(r))
          client.publish(accumulatedResponses): Unit
        }
      }

    }

    private def maybeAnswerClientFromLog(previousRound: Time): Unit = {
      log(s"log: ${state.log}")
      // println(s"${pprint.tokenize(newState).mkString("")}")

      for req @ Req(op, _) <- state.readDecisionsSince(previousRound) do {
        val decision: String = performOp(op)
        // println(s"queue size is: ${client.state.requests.size} / ${client.state.responses.size} (${distinctClients.size} clients)")
        // only leader is allowed to actually respond to requests
        if cluster.state.leader.contains(replicaId) then {
          client.publish {
            client.state.respond(req, decision)
          }: Unit
        }
      }

    }

  }

  // ============== CLIENT ==============

  class Client {
    @volatile var state: ClientState = RequestResponseQueue.empty

    given Lattice[Payload[ClientState]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    val dataManager: DeltaDissemination[ClientState] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncoming(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => currentStateLock.synchronized(state))
    )

    def handleIncoming(delta: ClientState): Unit = {
      log("handling incoming from client")
      val (old, changed) = currentStateLock.synchronized {
        val old = state
        state = state `merge` delta
        (old, state)
      }
      if old != changed then {
        assert(changed == state)
        cluster.maybeAnswerClientFromCache(state)
        cluster.maybeProposeNewValue(changed)
        cluster.forceUpkeep(): Unit
        // else log(s"upkept: ${pprint(upkept)}")
      }
    }

    def publish(delta: ClientState): ClientState = currentStateLock.synchronized {
      if delta `inflates` state then {
        log("publishing")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else
          log("skip")

      state
    }

  }

  // ============== CONN-INF ==============

  class ConnInf {
    @volatile var state: ConnInformation = HeartbeatQuorum()

    given Lattice[Payload[ConnInformation]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    var alivePeers: Set[Uid] = Set.empty

    val dataManager: DeltaDissemination[ConnInformation] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncoming(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => currentStateLock.synchronized(state))
    )

    def handleIncoming(delta: ConnInformation): Unit = {
      log(s"handling incoming conn inf: $delta")
      val (old, changed) = currentStateLock.synchronized {
        val receivedTime          = System.currentTimeMillis()
        val old                   = state
        val deltaWithReceivedTime = delta.copy(heartbeats = delta.heartbeats.map {
          case (id, LastWriterWins(t, Heartbeat(leader, senderTimestamp, _))) =>
            (id, LastWriterWins(t, Heartbeat(leader, senderTimestamp, Some(receivedTime))))
        })
        state = state `merge` deltaWithReceivedTime
        (old, state)
      }
    }

    def publish(delta: ConnInformation): ConnInformation = currentStateLock.synchronized {
      if delta `inflates` state then {
        log("publishing conn inf")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else log("skip publishing conn inf")

      state
    }

    def sendHeartbeat(): ConnInformation = {
      currentStateLock.synchronized {
        val heartbeat = Heartbeat(supposedLeader = cluster.state.leader, senderTimestamp = System.currentTimeMillis())
        publish(HeartbeatQuorum(Map(
          replicaId -> state.heartbeats.get(replicaId).map(_.write(heartbeat)).getOrElse(LastWriterWins.now(heartbeat))
        )))
      }
    }

    def checkLiveness(): Unit = {
      val newAlivePeers = state.alivePeers(timeoutThreshold, System.currentTimeMillis())

      (alivePeers -- newAlivePeers).foreach(id => println(s"Peer $id timed out"))
      alivePeers = newAlivePeers

      log(s"state: $state Alive peers: $alivePeers")

      if !alivePeers.exists(cluster.state.leader.contains) then {
        println(s"Detected leader failure (${cluster.state.leader}), triggering new election")
        cluster.maybeLeaderElection(alivePeers)
      }
    }

  }

}
