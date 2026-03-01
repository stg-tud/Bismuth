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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.atomic.AtomicReference

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
    if true then println(s"[$uid] $msg")

  val sendingActor: ExecutionContext = ConcurrencyHelper.makeExecutionContext(offloadSending)
  val replicaActor: ExecutionContext = ConcurrencyHelper.makeExecutionContext(offloadReplica)

  given Participants(votingReplicas)
  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef     = new {}
  val connInfStateLock: AnyRef     = new {}
  val clientReadStateLock: AnyRef  = new {}
  val clientWriteStateLock: AnyRef = new {}

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
        // else log(s"upkept: ${pprint(upkept)}")
        val newState = publish(upkept)
        maybeAnswerClientFromLog(old.nextDecisionRound, upkept)
        // try to propose a new value in case voting is decided
        maybeProposeNewValue()
      }
    }

    def publish(delta: ClusterState): ClusterState = currentStateLock.synchronized {
      if delta `inflates` state then {
        log(s"publishing cluster state $delta")
        val oldstate = state
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else {
        log("skip")
      }

      state
    }

    def forceUpkeep(): ClusterState = currentStateLock.synchronized {
      log("forcing upkeep")
      val upkept = publish(state.upkeep)
      maybeAnswerClientFromLog(state.nextDecisionRound, upkept)
      upkept
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

    def maybeProposeNewValue()(using LocalUid): Unit = currentStateLock.synchronized {
      // check if we are the leader and ready to handle a request
      if state.leader.contains(replicaId) && state.phase == MultipaxosPhase.Idle then

          client.nextProposal.get match
              case r @ Some(req) =>
                if client.nextProposal.compareAndSet(r, None) then
                  log(s"Proposing new value $req.")
                  client.writeQueue.updateAndGet(wq => wq.merge(wq.dequeueRequest(req)))
                  val proposal = state.proposeIfLeader(req)
                  if votingReplicas.size == 1 then {
                    val oldstate = state
                    state = state `merge` proposal
                    state = state `merge` state.upkeep
                    maybeAnswerClientFromLog(oldstate.nextDecisionRound, state): Unit
                  }
                  val _ = publish(proposal)
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

    def maybeAnswerClientFromCache(clientReadQueue: ClientState): Future[Unit] = {
      Future {
        // check if we are the leader and have a heartbeat quorum
        if !commitReads && state.leader.contains(replicaId) && connInf.state.hasQuorum(
              timeoutThreshold,
              System.currentTimeMillis()
            )
        then {
          // ready to propose value
          val responses = clientReadQueue.requests.queryAllEntries.collect {
            case req @ Req(k @ KVOperation.Read(key), _) =>
              clientReadQueue.respond(req, performOp(k))
          }

          if responses.nonEmpty then {
            log("answering unanswered read requests from cache")
            val accumulatedResponses = responses.fold(RequestResponseQueue.empty)((acc, r) => acc.merge(r))
            client.publishRead(accumulatedResponses): Unit
          }
        }
      }
    }

    private def maybeAnswerClientFromLog(previousRound: Time, state: ClusterState): Unit =  {
      log(s"log(${state.log.size}: ${state.log}")
      log(s"decisions since previous round ($previousRound): ${state.readDecisionsSince(previousRound).toList}")
      // println(s"${pprint.tokenize(newState).mkString("")}")

      for req @ Req(op, _) <- state.readDecisionsSince(previousRound) do {
        val decision: String = performOp(op)

        // println(s"queue size is: ${client.state.requests.size} / ${client.state.responses.size} (${distinctClients.size} clients)")
        // only leader is allowed to actually respond to requests
        if state.leader.contains(replicaId) then {
          op match {
            case KVOperation.Read(key) =>
              client.publishRead(client.readQueue.get.respond(req, decision)): Unit
            case KVOperation.Write(key, value) =>
              clientWriteStateLock.synchronized { client.publishWrite(client.writeQueue.get.respond(req, decision)) }: Unit
          }
        }
      }

    }

  }

  // ============== CLIENT ==============

  class Client {
    var writeQueue: AtomicReference[ClientState] = AtomicReference(RequestResponseQueue.empty)
    val readQueue: AtomicReference[ClientState]  = AtomicReference(RequestResponseQueue.empty)
    val nextProposal: AtomicReference[Option[Req[KVOperation[String, String]]]] = AtomicReference(None)

    given Lattice[Payload[ClientState]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    val dataManagerWrite: DeltaDissemination[ClientState] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncomingWrite(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => writeQueue.get())
    )
    val dataManagerRead: DeltaDissemination[ClientState] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncomingRead(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => readQueue.get())
    )

    def handleIncomingWrite(delta: ClientState): Unit = {
      val merged = writeQueue.updateAndGet(_.merge(delta))
      log("handling incoming write from client")
      findNextProposal()
      cluster.maybeProposeNewValue()
    }

    def handleIncomingRead(delta: ClientState): Unit = {
      val old    = readQueue.get()
      val merged = old `merge` delta
      if merged != old then
          log("handling incoming read from client")
          if readQueue.compareAndSet(old, merged) then
              cluster.maybeProposeNewValue()
          else
              handleIncomingRead(delta)
    }

    def findNextProposal() = {
      if nextProposal.get().isEmpty then {
        val request =
          if commitReads then {
            val writeRequest = writeQueue.get.firstUnansweredRequest // TODO: return from both queues here
            val readRequest = readQueue.get.firstUnansweredRequest // TODO: return from both queues here
          }
          else
            writeQueue.get.requestsSorted.collectFirst {
              case r@Req(KVOperation.Write(_, _), _) => r
            }

        request match {
          case Some(r) => nextProposal.compareAndSet(None, Some(r)) : Unit
          case None => ()
        }
      }
    }
    // ready to propose value
    val requestToAnswer = {
    }

    def publishWrite(delta: ClientState): ClientState =
      writeQueue.updateAndGet { wq =>
        if delta `inflates` wq then {
          log(s"publishing write delta $delta")
          dataManagerWrite.applyDelta(delta)
          wq.merge(delta)
        } else {
          log("skip")
          wq
        }
      }

    def publishRead(delta: ClientState): ClientState =
      readQueue.updateAndGet { rq =>
        if delta `inflates` rq then {
          log(s"publishing read delta $delta")
          dataManagerRead.applyDelta(delta)
          rq.merge(delta)
        } else {
          log("skip")
          rq
        }
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

    def publish(delta: ConnInformation): ConnInformation = connInfStateLock.synchronized {
      if delta `inflates` state then {
        log("publishing conn inf")
        state = state.merge(delta)
        dataManager.applyDelta(delta)
      } else log("skip publishing conn inf")

      state
    }

    def sendHeartbeat(): Future[ConnInformation] = Future {
      connInfStateLock.synchronized {
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
