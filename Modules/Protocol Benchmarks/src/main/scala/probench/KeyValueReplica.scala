package probench

import channels.ConcurrencyHelper
import probench.data.*
import probench.data.Codecs.given
import rdts.base.Lattice.syntax
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.protocols.Participants
import rdts.protocols.paper.{MultiPaxos, MultipaxosPhase}
import replication.DeltaStorage.Type.*
import replication.ProtocolMessage.Payload
import replication.{DeltaDissemination, DeltaStorage}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

//sealed trait ClientProtocol
//object ClientProtocol {
//  case class ClientRequest(req: Req[KVOperation[String, String]])                   extends ClientProtocol
//  case class ClusterAnswer(req: Req[KVOperation[String, String]], decision: String) extends ClientProtocol
//
//  given JsonValueCodec[ClientProtocol] = JsonCodecMaker.make
//}

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

  val sendingActor: ExecutionContext   = ConcurrencyHelper.makeExecutionContext(offloadSending)
  val replicaActor: ExecutionContext   = ConcurrencyHelper.makeExecutionContext(offloadReplica)
  val readReplyActor: ExecutionContext = ConcurrencyHelper.makeExecutionContext(false)

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
    Thread.sleep(2000) // wait 2 seconds before first leader election
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
        maybeAnswerClientFromLog(old.nextDecisionRound, newState)
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
          Option(client.writeQueue.poll()) match {
            case Some((_, req)) =>
              log(s"Proposing new value $req.")
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

    def maybeAnswerClientFromCache(): Unit = {
      // check if we are the leader and have a heartbeat quorum
      if !commitReads && state.leader.contains(replicaId) && connInf.state.hasQuorum(
            timeoutThreshold,
            System.currentTimeMillis()
          )
      then {
        var readOpt = Option(client.readQueue.poll())

        while readOpt.nonEmpty do
            readOpt match {
              case Some(_, ClientCommRead.ReadReq(id, op)) =>
                log("answering unanswered read request from cache")
                val response = performOp(op)
                client.publishRead(ClientCommRead.ReadRes(id, response))
                readOpt = Option(client.readQueue.poll())
              case None => ()
            }
      }
    }

    private def maybeAnswerClientFromLog(previousRound: Time, state: ClusterState): Unit = {
      log(s"log(${state.log.size}): ${state.log}")
      log(s"decisions since previous round ($previousRound): ${state.readDecisionsSince(previousRound).toList}")
      // println(s"${pprint.tokenize(newState).mkString("")}")

      for req @ ClientCommWrite.WriteReq(id, op) <- state.readDecisionsSince(previousRound) do {
        val result: String = performOp(op)

        // println(s"queue size is: ${client.state.requests.size} / ${client.state.responses.size} (${distinctClients.size} clients)")
        // only leader is allowed to actually respond to requests
        if state.leader.contains(replicaId) then {
          client.publishWrite(ClientCommWrite.WriteRes(id, result))
        }
      }

    }

  }

  // ============== CLIENT ==============

  type Time = Long
  class Client {
    import probench.data.ClientComm.given

    val readQueue: ConcurrentLinkedQueue[(Time, ClientCommRead.ReadReq)]    = ConcurrentLinkedQueue()
    val writeQueue: ConcurrentLinkedQueue[(Time, ClientCommWrite.WriteReq)] = ConcurrentLinkedQueue()
//    val nextProposal: AtomicReference[Option[ClientCommWrite.WriteReq]]     = AtomicReference(None)
//    val currentReads: AtomicReference[Set[ClientCommRead.ReadReq]]        = AtomicReference(Set.empty)

    val dataManagerWrite: DeltaDissemination[ClientCommWrite] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncomingWrite(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => ???)
    )
    val dataManagerRead: DeltaDissemination[ClientCommRead] = DeltaDissemination(
      localUid,
      delta => replicaActor.execute(() => handleIncomingRead(delta)),
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => ???)
    )

    def handleIncomingWrite(delta: ClientCommWrite): Unit = {
      delta match {
        case req @ ClientCommWrite.WriteReq(id, kvOperation) =>
          writeQueue.add((System.currentTimeMillis(), req))
          log("handling incoming write from client")
          cluster.maybeProposeNewValue()
        case ClientCommWrite.WriteRes(id, _) =>
          // clean queue asynchronously
          replicaActor.execute(() =>
            writeQueue.removeIf {
              case (_, ClientCommWrite.WriteReq(i, _)) => i == id
            } : Unit
          )
      }
    }

    def handleIncomingRead(delta: ClientCommRead): Unit = {
      delta match {
        case req @ ClientCommRead.ReadReq(id, kvOperation) =>
          readQueue.add((System.currentTimeMillis(), req))
          log("handling incoming read from client")
          readReplyActor.execute(() => cluster.maybeAnswerClientFromCache())
        case ClientCommRead.ReadRes(id, _) =>
          // clean queue asynchronously
          replicaActor.execute(() =>
            readQueue.removeIf {
              case (_, ClientCommRead.ReadReq(i, _)) => i == id
            }: Unit
          )
      }
    }

    def publishWrite(delta: ClientCommWrite.WriteRes): Unit =
      dataManagerWrite.applyDelta(delta)

    def publishRead(delta: ClientCommRead.ReadRes): Unit =
      dataManagerRead.applyDelta(delta)
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
      // log(s"handling incoming conn inf: $delta")
      val (old, changed) = currentStateLock.synchronized {
        val receivedTime          = System.currentTimeMillis()
        val old                   = state
        val deltaWithReceivedTime = delta.copy(heartbeats = delta.heartbeats.map {
          case (id, l @ LastWriterWins(t, Heartbeat(leader, senderTimestamp, _))) if id != replicaId =>
            (id, l.write(Heartbeat(leader, senderTimestamp, Some(receivedTime))))
          case h => h
        })
        state = state `merge` deltaWithReceivedTime
        (old, state)
      }
    }

    def publish(delta: ConnInformation): ConnInformation = connInfStateLock.synchronized {
      if delta `inflates` state then {
        // log("publishing conn inf")
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

      if !alivePeers.exists(cluster.state.leader.contains) then {
        println(
          s"Detected leader failure (${cluster.state.leader}) after $timeoutThreshold, alive peers $alivePeers triggering new election"
        )
        println(
          s"Detected leader failure (${cluster.state.leader}) after $timeoutThreshold, alive peers $alivePeers triggering new election"
        )
        cluster.maybeLeaderElection(alivePeers)
      }
    }

  }

}
