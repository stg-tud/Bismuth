package test.rdts.protocols.knowledgeGroups
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.LocalUid.replicaId
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.knowledgeGroups.MultiPaxos
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import replication.ProtocolMessage.Payload
import replication.*

class PaperGroups extends munit.FunSuite {
  val client    = Uid.predefined("client")
  val leader    = Uid.predefined("leader")
  val follower1 = Uid.predefined("follower1")
  val follower2 = Uid.predefined("follower2")
  val follower3 = Uid.predefined("follower3")
  val follower4 = Uid.predefined("follower4")
  val proxy     = Uid.predefined("proxy")
  val proxy1    = Uid.predefined("proxy1")
  val proxy2    = Uid.predefined("proxy2")

  def clientServerSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader and followers share slots
      ids = Set(leader, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share log
      ids = Set(client, leader),
      path = m => MultiPaxos[A](log = m.log),
      enabled = _ => true
    )
  ))

  def occamsRazorSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader and followers share slots
      ids = Set(leader, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          !paxos.currentRoundHasProposal // this is necessary such that this does not overlap with group 3
        )
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = m =>
        m.slots.isEmpty
    ),
    KnowledgeGroup( // everybody shares round2 votes // TODO: fix endless loop here. Only send to client, not to the rest...
      ids = Set(client, leader, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.currentRoundHasProposal
        )
    )
  ))

  def compartmentalizedSystem[A] = PrdtSystem[MultiPaxos[A]](Set(
    KnowledgeGroup( // leader proxy1 for uneven rounds, and then only phase2a or leaderElection
      ids = Set(leader, proxy1),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          m.slots.headOption.map(_._1 % 2 == 1).getOrElse(false) && (
            paxos.currentRound.map(_.leaderElection.votes.nonEmpty).getOrElse(false) ||
            paxos.currentRound.map(r => r.proposals.votes.map(_.voter) == Set(leader)).getOrElse(false)
          )
        )
    ),
    KnowledgeGroup( //  leader proxy2 for uneven rounds, and then only phase2a or leaderElection
      ids = Set(leader, proxy2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          m.slots.headOption.map(_._1 % 2 == 0).getOrElse(false) && (
            paxos.currentRound.map(_.leaderElection.votes.nonEmpty).getOrElse(false) ||
              paxos.currentRound.map(r => r.proposals.votes.map(_.voter) == Set(leader)).getOrElse(false)
            )
        )

    ),
    KnowledgeGroup( // proxy to uneven followers
      ids = Set(proxy1, follower1, follower3),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.headOption.map(_._1 % 2 == 1).getOrElse(false) ||
        m.log.nonEmpty
    ),
    KnowledgeGroup( // proxy followers as long as its undecided
      ids = Set(proxy2, follower2, follower4),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.headOption.map(_._1 % 2 == 0).getOrElse(false) ||
        m.log.nonEmpty
    ),
    KnowledgeGroup( // client leader requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = m => m.slots.isEmpty
    ),
    KnowledgeGroup( // client leader log
      ids = Set(client, leader),
      path = m => MultiPaxos[A](log = m.log),
      enabled = _ => true
    ),
    KnowledgeGroup( // leader proxies log
      ids = Set(leader, proxy1, proxy2),
      path = m => MultiPaxos[A](log = m.log),
      enabled = _ => true
    ),
  ))

  class Replica(
      id: LocalUid,
      participants: Set[Uid],
      systemConfig: PrdtSystem[MultiPaxos[Int]],
      var state: MultiPaxos[Int]
  ) {
    given LocalUid     = id
    given Participants = Participants(participants)

    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given Lattice[Payload[MultiPaxos[Int]]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    inline def log(inline msg: String): Unit =
      if true then println(s"[$replicaId] $msg")

    def handleDelta(delta: MultiPaxos[Int])(using Participants) = {
      state = state.merge(delta)
      val upkept = state.upkeep

      if !state.subsumes(upkept) then
          publish(upkept)
    }

    private def setupDataManagers: Map[Set[Uid], DeltaDissemination[MultiPaxos[Int]]] = {
      val uniqueEndpointSets = systemConfig.knowledgeGroups.map(_.ids).filter(_.contains(replicaId))

      uniqueEndpointSets.map(endpoints =>
        (
          endpoints,
          DeltaDissemination[MultiPaxos[Int]](
            id,
            delta => { publish(delta, Some(endpoints)); handleDelta(delta) },
            defaultTimetolive = 0,
            deltaStorage = DeltaStorage.getStorage(DeltaStorage.Type.KeepAll, () => ???)
          )
        )
      ).toMap
    }

    val dataManagers: Map[Set[Uid], DeltaDissemination[MultiPaxos[Int]]] = setupDataManagers

    def request(command: Int): Unit = {
      val delta = state.request(command)
      publish(delta)
    }

    def startLeaderElection(): Unit = {
      val delta = state.startLeaderElection(state.log.size)
      publish(delta)
    }

    def publish(delta: MultiPaxos[Int], source: Option[Set[Uid]] = None) = {
      state = state.merge(delta)

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

  test("ClientServerSystem Smoke Test") {
    val a = MultiPaxos[Int]()
    given Participants(Set(leader, follower1, follower2))
    given LocalUid(leader)
    val delta = a.startLeaderElection(0)

    assert(clientServerSystem.matches(delta, follower1))
    assert(clientServerSystem.matches(delta, leader))
  }

  test("test local connections with client-server knowledge groups") {
    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given Lattice[Payload[MultiPaxos[Int]]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived
    // given clusterCodec: JsonValueCodec[ProtocolMessage[MultiPaxos[Int]]] = JsonCodecMaker.make

    // val ids @ leader :: proxy :: client :: followers =
    //  List("leader", "proxy", "client", "follower1", "follower2").map(Uid.predefined): @unchecked
    val ids @ l :: c :: followers = List(leader, client, follower1, follower2): @unchecked
    given Participants(Set(leader, follower1, follower2))

    val systemConfig = clientServerSystem[Int]

    val replicas: Map[Uid, Replica] = ids.map(f =
      i =>
        (
          i,
          Replica(
            id = LocalUid(i),
            participants = Set(leader, follower1, follower2),
            systemConfig = systemConfig,
            state = MultiPaxos()
          )
        )
    ).toMap

    // setup connections
    // we need one data manager per set of ids. They can theoretically be reused between knowledge groups if they have the same set of ids
    // leader and followers share slots
    def setupConnections(allIds: Set[Uid]): Unit =
        def _setupConnections(remaining: Set[Uid]): Unit =
            val connection = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
            remaining.toList match {
              case primary :: Nil         => ()
              case primary :: secondaries =>
                println(s"Setting up connection for $allIds with $primary as server and $secondaries as clients")
                replicas(primary).dataManagers(allIds).addObjectConnection(connection.server)
                secondaries.foreach(id =>
                  replicas(id).dataManagers(allIds).addObjectConnection(connection.client(id.toString))
                )
                _setupConnections(secondaries.toSet)
              case Nil => ()
            }
        _setupConnections(allIds)

    systemConfig.knowledgeGroups.map(_.ids).foreach(setupConnections)

    assertEquals(replicas(leader).dataManagers.size, 2)
    assertEquals(replicas(follower1).dataManagers.size, 1)

    replicas(client).request(0)
    assert(!replicas(client).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.requests.elements.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    replicas(leader).startLeaderElection()
    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    assertEquals(replicas(leader).state.read, Seq(0))
    assertEquals(replicas(client).state.read, Seq(0))
    assertEquals(replicas(follower1).state.read, Seq(0))
    assertEquals(replicas(follower2).state.read, Seq(0))

    replicas(client).request(1)
    assertEquals(replicas(leader).state.read, Seq(0, 1))
    assertEquals(replicas(client).state.read, Seq(0, 1))
    assertEquals(replicas(follower1).state.read, Seq(0, 1))
    assertEquals(replicas(follower2).state.read, Seq(0, 1))
    assertEquals(replicas(leader).state, replicas(follower1).state)
    assertEquals(replicas(follower2).state, replicas(follower1).state)
    assertNotEquals(replicas(leader).state, replicas(client).state)
  }

  test("test local connections with occam's razor groups") {
    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given Lattice[Payload[MultiPaxos[Int]]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived
    // given clusterCodec: JsonValueCodec[ProtocolMessage[MultiPaxos[Int]]] = JsonCodecMaker.make

    // val ids @ leader :: proxy :: client :: followers =
    //  List("leader", "proxy", "client", "follower1", "follower2").map(Uid.predefined): @unchecked
    val ids @ l :: c :: followers = List(leader, client, follower1, follower2): @unchecked
    given Participants(Set(leader, follower1, follower2))

    val systemConfig = occamsRazorSystem[Int]

    val replicas: Map[Uid, Replica] = ids.map(f =
      i =>
        (
          i,
          Replica(
            id = LocalUid(i),
            participants = Set(leader, follower1, follower2),
            systemConfig = systemConfig,
            state = MultiPaxos()
          )
        )
    ).toMap

    // setup connections
    // we need one data manager per set of ids. They can theoretically be reused between knowledge groups if they have the same set of ids
    // leader and followers share slots
    def setupConnections(allIds: Set[Uid]): Unit =
        def _setupConnections(remaining: Set[Uid]): Unit =
            val connection = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
            remaining.toList match {
              case primary :: Nil         => ()
              case primary :: secondaries =>
                println(s"Setting up connection for $allIds with $primary as server and $secondaries as clients")
                replicas(primary).dataManagers(allIds).addObjectConnection(connection.server)
                secondaries.foreach(id =>
                  replicas(id).dataManagers(allIds).addObjectConnection(connection.client(id.toString))
                )
                _setupConnections(secondaries.toSet)
              case Nil => ()
            }
        _setupConnections(allIds)

    systemConfig.knowledgeGroups.map(_.ids).foreach(setupConnections)

    assertEquals(replicas(leader).dataManagers.size, 3)
    assertEquals(replicas(follower1).dataManagers.size, 2)

    replicas(client).request(0)
    assert(!replicas(client).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.requests.elements.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    replicas(leader).startLeaderElection()
    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    assertEquals(replicas(leader).state.read, Seq(0))
    assertEquals(replicas(client).state.read, Seq(0))
    assertEquals(replicas(follower1).state.read, Seq(0))
    assertEquals(replicas(follower2).state.read, Seq(0))

    replicas(client).request(1)
    assertEquals(replicas(leader).state.read, Seq(0, 1))
    assertEquals(replicas(client).state.read, Seq(0, 1))
    assertEquals(replicas(follower1).state.read, Seq(0, 1))
    assertEquals(replicas(follower2).state.read, Seq(0, 1))

    assertEquals(replicas(leader).state, replicas(follower1).state)
    assertEquals(replicas(follower2).state, replicas(follower1).state)
    assertNotEquals(replicas(leader).state, replicas(client).state)
  }

  test("test local connections with compartmentalization") {
    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given Lattice[Payload[MultiPaxos[Int]]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived
    // given clusterCodec: JsonValueCodec[ProtocolMessage[MultiPaxos[Int]]] = JsonCodecMaker.make

    // val ids @ leader :: proxy :: client :: followers =
    //  List("leader", "proxy", "client", "follower1", "follower2").map(Uid.predefined): @unchecked
    val ids @ l :: c :: followers =
      List(leader, proxy1, proxy2, client, follower1, follower2, follower3, follower4): @unchecked
    given Participants(Set(leader, follower1, follower2, follower3, follower4))

    val systemConfig = compartmentalizedSystem[Int]

    val replicas: Map[Uid, Replica] = ids.map(f =
      i =>
        (
          i,
          Replica(
            id = LocalUid(i),
            participants = Set(leader, follower1, follower2, follower3, follower4),
            systemConfig = systemConfig,
            state = MultiPaxos()
          )
        )
    ).toMap

    // setup connections
    // we need one data manager per set of ids. They can theoretically be reused between knowledge groups if they have the same set of ids
    // leader and followers share slots
    def setupConnections(allIds: Set[Uid]): Unit =
        def _setupConnections(remaining: Set[Uid]): Unit =
            val connection = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
            remaining.toList match {
              case primary :: Nil         => ()
              case primary :: secondaries =>
                println(s"Setting up connection for $allIds with $primary as server and $secondaries as clients")
                replicas(primary).dataManagers(allIds).addObjectConnection(connection.server)
                secondaries.foreach(id =>
                  replicas(id).dataManagers(allIds).addObjectConnection(connection.client(id.toString))
                )
                _setupConnections(secondaries.toSet)
              case Nil => ()
            }
        _setupConnections(allIds)

    systemConfig.knowledgeGroups.map(_.ids).foreach(setupConnections)

    assertEquals(replicas(leader).dataManagers.size, 4)
    assertEquals(replicas(proxy1).dataManagers.size, 3)
    assertEquals(replicas(follower1).dataManagers.size, 1)
    assertEquals(replicas(client).dataManagers.size, 1)

    replicas(client).request(0)
    assert(!replicas(client).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.requests.elements.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    replicas(leader).startLeaderElection()
    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    assertEquals(replicas(leader).state.read, Seq(0))
    assertEquals(replicas(client).state.read, Seq(0))
    assertEquals(replicas(follower1).state.read, Seq(0))
    assertEquals(replicas(follower2).state.read, Seq(0))

    replicas(client).request(1)
    assertEquals(replicas(leader).state.read, Seq(0, 1))
    assertEquals(replicas(client).state.read, Seq(0, 1))
    assertEquals(replicas(follower1).state.read, Seq(0, 1))
    assertEquals(replicas(follower2).state.read, Seq(0, 1))

    assertNotEquals(replicas(leader).state, replicas(client).state)
  }

}
