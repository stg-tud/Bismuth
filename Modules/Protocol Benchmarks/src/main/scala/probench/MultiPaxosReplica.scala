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

  val compartmentalized = PrdtSystem[MultiPaxos[Request]](Set(
    KnowledgeGroup( // leader proxy1 for uneven rounds, and then only phase2a or leaderElection
      ids = Set(leader, proxy1),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          m.slots.headOption.map(_._1 % 2 == 1).getOrElse(false) && (
            paxos.currentRound.map(_.leaderElection.votes.nonEmpty).getOrElse(false) ||
            paxos.currentRound.map(r => r.proposals.votes.map(_.voter) == Set(leader)).getOrElse(false)
          )
        )
        || m.log.nonEmpty
    ),
    KnowledgeGroup( //  leader proxy2 for uneven rounds, and then only phase2a or leaderElection
      ids = Set(leader, proxy2),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          m.slots.headOption.map(_._1 % 2 == 0).getOrElse(false) && (
            paxos.currentRound.map(_.leaderElection.votes.nonEmpty).getOrElse(false) ||
            paxos.currentRound.map(r => r.proposals.votes.map(_.voter) == Set(leader)).getOrElse(false)
          )
        )
        || m.log.nonEmpty
    ),
    KnowledgeGroup( // proxy to uneven followers
      ids = Set(proxy1, follower1, follower3),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.headOption.map(_._1 % 2 == 1).getOrElse(false) ||
        m.log.nonEmpty
    ),
    KnowledgeGroup( // proxy followers as long as its undecided
      ids = Set(proxy2, follower2, follower4),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.headOption.map(_._1 % 2 == 0).getOrElse(false) ||
        m.log.nonEmpty
    ),
    KnowledgeGroup( // client leader requests
      ids = Set(client, leader),
      path = m => MultiPaxos[Request](requests = m.requests),
      enabled = m => m.slots.isEmpty
    ),
    KnowledgeGroup( // client leader log
      ids = Set(client, leader),
      path = m => MultiPaxos[Request](log = m.log),
      enabled = _ => true
    ),
  ))

  val occamsRazor = PrdtSystem[MultiPaxos[Request]](Set(
    KnowledgeGroup( // leader and followers share slots
      ids = Set(leader, follower1, follower2, follower3, follower4),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          !paxos.currentRoundHasProposal // this is necessary such that this does not overlap with group 3
        )
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[Request](requests = m.requests),
      enabled = m =>
        m.slots.isEmpty
    ),
    KnowledgeGroup( // everybody shares round2 votes // TODO: fix endless loop here. Only send to client, not to the rest...
      ids = Set(client, leader, follower1, follower2, follower3, follower4),
      path = m => MultiPaxos[Request](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.currentRoundHasProposal
        )
    )
  ))
}

class MultiPaxosReplica(
    id: Uid,
    participants: Set[Uid],
    val systemConfig: PrdtSystem[MultiPaxos[Request]],
    var state: MultiPaxos[Request],

) {
  given LocalUid     = LocalUid(id)
  given Participants = Participants(participants)

  val currentStateLock: AnyRef = new {}

  private val promises: mutable.HashMap[Uid, Promise[String]] = mutable.HashMap.empty[Uid, Promise[String]]

  inline def log(inline msg: String): Unit =
    if true then println(s"[$replicaId] $msg")

  def handleDelta(delta: MultiPaxos[Request])(using Participants) = {
    log(s"received delta: $delta")
    maybeReturnResult(delta)
    currentStateLock.synchronized {
      state = state.merge(delta)
      val upkept = state.upkeep
      maybeReturnResult(upkept)

      if !state.subsumes(upkept) then
          publish(upkept)
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

  def requestWithResult(requestId: Uid, payload: String): Future[String] = {
    val delta = state.request(Request(requestId, payload))
    val p     = Promise[String]()

    promises.synchronized {
      promises.put(requestId, p)
      log("adding promise")
    }
    publish(delta)
    p.future
  }

  private def maybeReturnResult(delta: MultiPaxos[Request]): Unit = {
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

  def startLeaderElection(): Unit = {
    val delta = state.startLeaderElection(state.log.size)
    publish(delta)
  }

  def publish(delta: MultiPaxos[Request], source: Option[Set[Uid]] = None) = {
    currentStateLock.synchronized {
      state = state.merge(delta)
    }
    // log(s"trying to publish $delta")

    dataManagers.foreach {
      case (uids, dataManager) =>
        if !source.contains(uids) && // don't forward deltas to the knowledge groups they are coming from
            systemConfig.matches(delta, uids)
        then
            log(s"sending delta $delta to $uids")
            dataManager.applyDelta(delta)
//        else
//          log(s"no match for $delta")
    }
  }

}
