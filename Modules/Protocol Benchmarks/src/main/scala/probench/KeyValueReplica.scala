package probench

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

import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext

class KeyValueReplica(
    val uid: Uid,
    val votingReplicas: Set[Uid],
    offloadSending: Boolean = true,
    deltaStorageType: DeltaStorage.Type = KeepAll,
    timeoutThreshold: Long = 1000
) {

  inline def log(inline msg: String): Unit =
    if false then println(s"[$uid] $msg")

  val sendingActor: ExecutionContext = makeActor(offloadSending)
  val replicaActor: ExecutionContext = makeActor(true)

  private def makeActor(offloadSending: Boolean) = {
    if offloadSending then
        val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor { r =>
          val thread = new Thread(r)
          thread.setDaemon(true)
          thread
        }

        ExecutionContext.fromExecutorService(singleThreadExecutor)
    else
        DeltaDissemination.executeImmediately
  }

  given Participants(votingReplicas)
  given localUid: LocalUid = LocalUid(uid)

  val currentStateLock: AnyRef = new {}

  private val kvCache = mutable.HashMap[String, String]()

  // ============== CLUSTER ==============

  val cluster: Cluster = new Cluster(localUid, sendingActor)
  val client: Client   = new Client(localUid, sendingActor)
  val connInf: ConnInf = new ConnInf(localUid, sendingActor, timeoutThreshold = timeoutThreshold)

  cluster.maybeLeaderElection(votingReplicas)

  class Cluster(
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ClusterState = MultiPaxos.empty,
  ) {

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
        maybeAnswerClient(old.rounds.counter)
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
        maybeAnswerClient(oldstate.rounds.counter)
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
          client.firstUnansweredRequest match
              case Some(req) =>
                log(s"Proposing new value $req.")
                val _ = publish(state.proposeIfLeader(req))
              case None =>
                log("I am the leader but request queue is empty.")
    }

    private def maybeAnswerClient(previousRound: Time): Unit = {
      log(s"log: ${state.log}")
      // println(s"${pprint.tokenize(newState).mkString("")}")

      for req @ Req(op, _, _) <- state.readDecisionsSince(previousRound) do {
        val decision: String = op match {
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

  class Client(
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ClientState = RequestResponseQueue.empty
  ) {

    given Lattice[Payload[ClientState]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    val dataManager: DeltaDissemination[ClientState] = DeltaDissemination(
      localUid,
      handleIncoming,
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

  class ConnInf(
      localUid: LocalUid,
      sendingActor: ExecutionContext,
      var state: ConnInformation = Map.empty,
      val timeoutThreshold: Long
  ) {

    given Lattice[Payload[ConnInformation]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    var alivePeers: Set[Uid] = Set.empty

    val dataManager: DeltaDissemination[ConnInformation] = DeltaDissemination(
      localUid,
      handleIncoming,
      defaultTimetolive = 0,
      sendingActor = sendingActor,
      deltaStorage = DeltaStorage.getStorage(deltaStorageType, () => currentStateLock.synchronized(state))
    )

    def handleIncoming(delta: ConnInformation): Unit = {
      log("handling incoming conn inf")
      val (old, changed) = currentStateLock.synchronized {
        val old = state
        state = state `merge` delta
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
        publish(Map.from(List((localUid, LastWriterWins.now(System.currentTimeMillis())))))
      }
    }

    def checkLiveness(): Unit = {
      val newAlivePeers = state
        .filter((_, lww) => lww.value >= (System.currentTimeMillis() - timeoutThreshold))
        .map((uid, _) => uid.uid)
        .toSet

      (alivePeers -- newAlivePeers).foreach(id => println(s"Peer $id timed out"))
      alivePeers = newAlivePeers

      log(s"Alive peers: $alivePeers")

      if !alivePeers.exists(cluster.state.leader.contains) then {
        println("Detected leader failure, triggering new election")
        cluster.maybeLeaderElection(alivePeers)
      }
    }

  }

}
