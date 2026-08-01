package test.rdts.protocols.knowledgeGroups
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.protocols.Consensus.lattice
import rdts.protocols.Paxos.given_Ordering_BallotNum_PaxosRound
import rdts.protocols.Util.Agreement
import rdts.protocols.Util.Agreement.Undecided
import rdts.protocols.{Participants, Paxos, PaxosRound, Voting}
import rdts.protocols.knowledgeGroups.MultiPaxos
import replication.ProtocolMessage.Payload
import replication.{DeltaDissemination, DeltaStorage, KnowledgeGroup, PrdtSystem, ProtocolMessage}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.math.Ordering.comparatorToOrdering

class PaperGroups extends munit.FunSuite {
  val client    = Uid.predefined("client")
  val leader    = Uid.predefined("leader")
  val follower1 = Uid.predefined("follower1")
  val follower2 = Uid.predefined("follower2")
  val proxy     = Uid.predefined("proxy")

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
    KnowledgeGroup( // leader proxy is phase2a or leaderElection
      ids = Set(leader, proxy),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.currentRound.flatMap(_.proposals.votes.headOption.map(_.voter == leader)).getOrElse(false) ||
          paxos.currentRound.flatMap(_.leaderElection.votes.headOption.map(_.voter == leader)).getOrElse(false)
        )
    ),
    KnowledgeGroup( // leader proxy is decided
      ids = Set(leader, proxy),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
        )
    ),
    KnowledgeGroup( // proxy followers as long as its undecided
      ids = Set(proxy, follower1, follower2),
      path = m => MultiPaxos[A](slots = m.slots),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.decision(using Participants(Set(leader, follower1, follower2))) == Agreement.Undecided
        )
    ),
    KnowledgeGroup( // client leader requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // client leader log
      ids = Set(client, leader),
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

  test("Compartmentalization") {
    var paxosLeader      = MultiPaxos[Int]()
    var paxosFollower1   = MultiPaxos[Int]()
    var paxosFollower2   = MultiPaxos[Int]()
    var paxosProxy       = MultiPaxos[Int]()
    var proxyDeltaBuffer = MultiPaxos[Int]()
    given Participants(Set(leader, follower1, follower2))

    val d1 = {
      given LocalUid(leader)

      //// start leader election
      val delta = paxosLeader.startLeaderElection(0)
      // should match only proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(compartmentalizedSystem.matches(delta, leader))
      assert(!compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))

      paxosLeader = paxosLeader.merge(delta)
      paxosLeader = paxosLeader.merge(paxosLeader.upkeep)
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d1)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d1, proxy))
      assert(compartmentalizedSystem.matches(d1, leader))
      assert(compartmentalizedSystem.matches(d1, follower1))
      assert(compartmentalizedSystem.matches(d1, follower1))
      assert(!compartmentalizedSystem.matches(d1, client))
    }
    val d2 = {
      // delta from before is received by follower1
      given LocalUid(follower1)
      paxosFollower1 = paxosFollower1.merge(d1)
      // perform upkeep -> vote in leader election
      val delta = paxosFollower1.upkeep
      paxosFollower1 = paxosFollower1.merge(delta)

      // upkeep delta should match only follower2 and proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(!compartmentalizedSystem.matches(delta, leader))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d2)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d2, proxy))
      assert(!compartmentalizedSystem.matches(d2, leader))
      assert(compartmentalizedSystem.matches(d2, follower1))
      assert(compartmentalizedSystem.matches(d2, follower1))
      assert(!compartmentalizedSystem.matches(d2, client))
    }
    val d3 = {
      // leader receives leader election delta and starts proposal
      given LocalUid(leader)
      paxosLeader = paxosLeader.merge(d2)

      //// start leader election
      val delta = paxosLeader.proposeIfLeader(0, 0)
      // should match only proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(compartmentalizedSystem.matches(delta, leader))
      assert(!compartmentalizedSystem.matches(delta, follower1))
      assert(!compartmentalizedSystem.matches(delta, client))

      paxosLeader = paxosLeader.merge(delta)
      paxosLeader = paxosLeader.merge(paxosLeader.upkeep)
      delta
    }
    {
      // delta from before can be forwarded by proxy
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d3)
      proxyDeltaBuffer = proxyDeltaBuffer.merge(d3)

      // should match only leader and followers
      assert(compartmentalizedSystem.matches(d3, proxy))
      assert(compartmentalizedSystem.matches(d3, leader))
      assert(compartmentalizedSystem.matches(d3, follower1))
      assert(compartmentalizedSystem.matches(d3, follower2))
      assert(!compartmentalizedSystem.matches(d3, client))
    }
    val d4 = {
      // delta from before is received by follower1
      given LocalUid(follower1)
      paxosFollower1 = paxosFollower1.merge(d3)
      // perform upkeep -> accept proposal
      val delta = paxosFollower1.upkeep
      paxosFollower1 = paxosFollower1.merge(delta)

      assertEquals(paxosFollower1.read, Seq(0))
      assertNotEquals(paxosLeader.read, Seq(0))

      // upkeep delta should match only follower2 and proxy
      assert(compartmentalizedSystem.matches(delta, proxy))
      assert(!compartmentalizedSystem.matches(delta, leader))
      assert(compartmentalizedSystem.matches(delta, follower1))
      assert(compartmentalizedSystem.matches(delta, follower2))
      assert(!compartmentalizedSystem.matches(delta, client))
      delta
    }
    val d5 = {
      // delta from before cannot yet be forwarded by proxy because it does not contain the decision
      given LocalUid(proxy)
      paxosProxy = paxosProxy.merge(d4)
      proxyDeltaBuffer = proxyDeltaBuffer.merge(d4)

      val proxyGroup =
        KnowledgeGroup[MultiPaxos[Int]]( // leader proxy is decided
          ids = Set(leader, proxy),
          path = m => MultiPaxos[Int](slots = m.slots),
          enabled = m =>
            m.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
        )
      // should match only proxy and followers
      assert(compartmentalizedSystem.matches(d4, proxy))
      assert(!compartmentalizedSystem.matches(d4, leader))
      assert(compartmentalizedSystem.matches(d4, follower1))
      assert(compartmentalizedSystem.matches(d4, follower2))
      assert(!compartmentalizedSystem.matches(d4, client))

      // however, we can construct a delta that contains the decision
      val currentSlot = paxosProxy.slots.size.toLong - 1
      val paxosDelta  = {
        val currRound = paxosProxy.slots(currentSlot).rounds.maxOption
        currRound.map((ballot, round) => Paxos(Map(ballot -> PaxosRound(proposals = round.proposals))))
      }.getOrElse(Paxos())
      val decDelta = MultiPaxos(slots = Map(currentSlot -> paxosDelta))

      assert(!Bottom[MultiPaxos[Int]].isEmpty(decDelta))
      assert(compartmentalizedSystem.matches(decDelta, leader))
      assert(!compartmentalizedSystem.matches(decDelta, follower1))
      assert(!compartmentalizedSystem.matches(decDelta, follower2))
      assert(!compartmentalizedSystem.matches(decDelta, client))
      assert(paxosProxy.subsumes(decDelta)) // this is safe because we only repackage existing knowledge

      // alternatively, we can use the buffered deltas from the follower
      assert(compartmentalizedSystem.matches(proxyDeltaBuffer, leader))
      assert(paxosProxy.subsumes(proxyDeltaBuffer)) // this is safe because we only repackage existing knowledge

      decDelta
    }
    {
      // compiled delta from proxy is received by leader
      given LocalUid(leader)
      paxosLeader = paxosLeader.merge(d5)
      paxosLeader = paxosLeader.merge(paxosLeader.upkeep)

      assertEquals(paxosLeader.read, Seq(0))
      assertEquals(paxosLeader.requests.elements, Set.empty)
    }
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
          case primary :: Nil => ()
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
    assertEquals(replicas(leader).state.read, Seq(0,1))
    assertEquals(replicas(client).state.read, Seq(0,1))
    assertEquals(replicas(follower1).state.read, Seq(0,1))
    assertEquals(replicas(follower2).state.read, Seq(0,1))
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
          case primary :: Nil => ()
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
    assertEquals(replicas(leader).state.read, Seq(0,1))
    assertEquals(replicas(client).state.read, Seq(0,1))
    assertEquals(replicas(follower1).state.read, Seq(0,1))
    assertEquals(replicas(follower2).state.read, Seq(0,1))

    assertEquals(replicas(leader).state, replicas(follower1).state)
    assertEquals(replicas(follower2).state, replicas(follower1).state)
    assertNotEquals(replicas(leader).state, replicas(client).state)
  }

}
