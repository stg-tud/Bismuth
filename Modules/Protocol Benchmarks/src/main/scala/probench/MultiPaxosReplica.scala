package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.knowledgeGroups.MultiPaxos
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import replication.ProtocolMessage.Payload
import replication.{DeltaDissemination, DeltaStorage, KnowledgeGroup, PrdtSystem, ProtocolMessage}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

case class Request(id: Uid, payload: String)

given JsonValueCodec[MultiPaxos[Request]] =
  JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

given Lattice[Payload[MultiPaxos[Request]]] =
    given Lattice[Int] = Lattice.fromOrdering

    Lattice.derived

object KnowledgeGroups {
  val client    = Uid.predefined("client")
  val leader    = Uid.predefined("leader")
  val follower1 = Uid.predefined("follower1")
  val follower2 = Uid.predefined("follower2")
  val follower3 = Uid.predefined("follower3")
  val follower4 = Uid.predefined("follower4")
  val proxy1    = Uid.predefined("proxy1")
  val proxy2    = Uid.predefined("proxy2")
  // client server config
  val clientServer = {
    PrdtSystem[MultiPaxos[Request]](Set(
      KnowledgeGroup( // leader and followers share slots
        ids = Set(leader, follower1, follower2, follower3, follower4),
        path = m => MultiPaxos[Request](slots = m.slots),
        enabled = _ => true
      ),
      KnowledgeGroup( // client and leader share requests
        ids = Set(client, leader),
        path = m => MultiPaxos[Request](requests = m.requests),
        enabled = _ => true
      ),
      KnowledgeGroup( // client and leader share log
        ids = Set(client, leader),
        path = m => MultiPaxos[Request](log = m.log),
        enabled = _ => true
      )
    ))
  }
}

class MultiPaxosReplica(
    id: Uid,
    participants: Set[Uid],
    systemConfig: PrdtSystem[MultiPaxos[Request]],
    var state: MultiPaxos[Request],

) {
  given LocalUid     = LocalUid(id)
  given Participants = Participants(participants)

  val currentStateLock: AnyRef = new {}

  private val promises: mutable.HashMap[Uid, Promise[String]] = mutable.HashMap.empty[Uid, Promise[String]]

  inline def log(inline msg: String): Unit =
    if false then println(s"[$replicaId] $msg")

  def handleDelta(delta: MultiPaxos[Request])(using Participants) = {
    currentStateLock.synchronized {
      state = state.merge(delta)
      val upkept = state.upkeep

      if !state.subsumes(upkept) then
          publish(upkept)
    }

    // return resolved requests
    promises.synchronized {
      val answers = delta.log.map(_._2)
      answers.foreach {
        case Request(id, payload) => promises.remove(id) match {
            case Some(promise) => promise.success(payload): Unit
            case None          => ()
          }
      }
    }
  }

  lazy val dataManagers: Map[Set[Uid], DeltaDissemination[MultiPaxos[Request]]] = {
    val uniqueEndpointSets = systemConfig.knowledgeGroups.map(_.ids).filter(_.contains(replicaId))

    uniqueEndpointSets.map(endpoints =>
      (
        endpoints,
        DeltaDissemination[MultiPaxos[Request]](
          LocalUid(id),
          delta => { publish(delta, Some(endpoints)); handleDelta(delta) },
          defaultTimetolive = 0,
          deltaStorage = DeltaStorage.getStorage(DeltaStorage.Type.KeepAll, () => ???)
        )
      )
    ).toMap
  }

  def request(payload: String): Unit = {
    val delta = state.request(Request(Uid.gen(), payload))
    publish(delta)
  }

  def requestWithResult(payload: String): Future[String] = {
    val requestId = Uid.gen()
    val delta     = state.request(Request(requestId, payload))
    val p         = Promise[String]()

    promises.synchronized {
      promises.put(requestId, p)
      log("adding promise")
    }
    publish(delta)
    p.future
  }

  def startLeaderElection(): Unit = {
    val delta = state.startLeaderElection(state.log.size)
    publish(delta)
  }

  def publish(delta: MultiPaxos[Request], source: Option[Set[Uid]] = None) = {
    currentStateLock.synchronized {
      state = state.merge(delta)
    }

    dataManagers.foreach {
      case (uids, dataManager) =>
        if !source.contains(uids) && // don't forward deltas to the knowledge groups they are coming from
            systemConfig.matches(delta, uids)
        then
            log(s"sending delta $delta to $uids")
            dataManager.applyDelta(delta)
    }
  }

}
