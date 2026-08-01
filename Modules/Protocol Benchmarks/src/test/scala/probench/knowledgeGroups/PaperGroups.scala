package test.rdts.protocols.knowledgeGroups
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
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
      // todo: only send back to client when request has been fulfilled
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
      enabled = _ => true
    ),
    KnowledgeGroup( // client and leader share requests
      ids = Set(client, leader),
      path = m => MultiPaxos[A](requests = m.requests),
      enabled = _ => true
    ),
    KnowledgeGroup( // everybody shares decisions
      ids = Set(client, leader, follower1, follower2),
      path = m => MultiPaxos[A](log = m.log),
      enabled = m =>
        m.slots.forall((id, paxos) =>
          paxos.decision(using Participants(Set(leader, follower1, follower2))) != Agreement.Undecided
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

  class Replica(id: LocalUid, participants: Set[Uid], var state: MultiPaxos[Int]) {
    given LocalUid     = id
    given Participants = Participants(participants)

    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given Lattice[Payload[MultiPaxos[Int]]] =
        given Lattice[Int] = Lattice.fromOrdering
        Lattice.derived

    def handleDelta(delta: MultiPaxos[Int])(using Participants) = {
      state = state.merge(delta)
      val upkept = state.upkeep

      if !state.subsumes(upkept) then
          publish(upkept)
    }

    val systemConfig = clientServerSystem[Int]

    // leader and followers share slots
    val dataManager1: DeltaDissemination[MultiPaxos[Int]] = DeltaDissemination(
      id,
      delta => handleDelta(delta),
      defaultTimetolive = 0,
      deltaStorage = DeltaStorage.getStorage(DeltaStorage.Type.KeepAll, () => ???)
    )
    // client and leader share requests + log
    val dataManager2: DeltaDissemination[MultiPaxos[Int]] = DeltaDissemination(
      id,
      delta => handleDelta(delta),
      defaultTimetolive = 0,
      deltaStorage = DeltaStorage.getStorage(DeltaStorage.Type.KeepAll, () => ???)
    )

    def request(command: Int): Unit = {
      val delta = state.request(command)
      publish(delta)
    }

    def startLeaderElection(): Unit = {
      val delta = state.startLeaderElection(state.log.size)
      publish(delta)
    }

    def publish(delta: MultiPaxos[Int]) = {
      state = state.merge(delta)
      if systemConfig.matches(delta, Set(leader, follower1, follower2)) then
          dataManager1.applyDelta(delta)

      if systemConfig.matches(delta, Set(leader, client)) then
          dataManager2.applyDelta(delta)
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

  test("consensus with knowledge groups") {
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

    val replicas: Map[Uid, Replica] = ids.map(f =
      i => (i, Replica(id = LocalUid(i), participants = Set(leader, follower1, follower2), state = MultiPaxos()))
    ).toMap

    // setup connections // TODO: maybe do this automatically based on the knowledge groups?
    // we need one data manager per set of ids. They can theoretically be reused between knowledge groups if they have the same set of ids
    // leader and followers share slots
    val connection1 = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
    replicas(leader).dataManager1.addObjectConnection(connection1.server)
    followers.foreach(id => replicas(id).dataManager1.addObjectConnection(connection1.client(id.toString)))
    val connection2 = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
    replicas(followers(0)).dataManager1.addObjectConnection(connection2.server)
    replicas(followers(1)).dataManager1.addObjectConnection(connection2.client(followers(1).toString))

    // client and leader share requests + log
    val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[MultiPaxos[Int]]]()
    replicas(leader).dataManager2.addObjectConnection(clientConnection.server)
    replicas(client).dataManager2.addObjectConnection(clientConnection.client(client.toString))

    replicas(client).request(0)
    assert(!replicas(client).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.requests.elements.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    replicas(leader).startLeaderElection()
    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)
//    client.printResults = false
//
//    client.write("test", "Hi")
//    client.read("test")
//
//    assertEquals(nodes(0).cluster.state, nodes(1).cluster.state)
//    assertEquals(nodes(1).cluster.state, nodes(2).cluster.state)
//    assertEquals(nodes(2).cluster.state, nodes(0).cluster.state)
//
//    def investigateUpkeep(state: ClusterState)(using LocalUid) = {
//      val delta  = state.upkeep
//      val merged = state `merge` delta
//      assert(state != merged)
//      assert(delta `inflates` state, delta)
//    }
//
//    def runUpkeep() = while {
//      nodes.filter(_.cluster.needsUpkeep()).exists { n =>
//        investigateUpkeep(n.cluster.state)(using n.localUid)
//        n.cluster.forceUpkeep()
//        true
//      }
//    } do ()
//
//    runUpkeep()
//
//    nodes.foreach(node => assert(!node.cluster.needsUpkeep(), node.uid))
//
//    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
//      val current = keyValueReplica.cluster.state
//      assertEquals(
//        current `merge` current.upkeep(using keyValueReplica.localUid),
//        current,
//        s"${keyValueReplica.uid} upkeep"
//      )
//    }
//
//    nodes.foreach(noUpkeep)
//
//    assertEquals(nodes(0).cluster.state, nodes(1).cluster.state)
//    assertEquals(nodes(1).cluster.state, nodes(2).cluster.state)
//    assertEquals(nodes(2).cluster.state, nodes(0).cluster.state)
//
//    // simulate crash
//
//    secondaries.last.cluster.dataManager.globalAbort.closeRequest = true
//
//    client.printResults = false
//
//    client.write("test2", "Hi")
//    client.read("test2")
//
//    runUpkeep()
//
//    nodes.foreach(noUpkeep)
//
//    assertEquals(nodes(0).cluster.state.closedRounds(1)._2, KVOperation.Write("test2", "Hi"))
//    assertEquals(nodes(2).cluster.state.closedRounds.size, 1)

  }

}
