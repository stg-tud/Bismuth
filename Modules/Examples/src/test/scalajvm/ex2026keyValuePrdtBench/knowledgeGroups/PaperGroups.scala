package test.rdts.protocols.knowledgeGroups
import ex2026keyValuePrdtBench.{KnowledgeGroups, Request}
import rdts.base.{ReplicaId, Uid}
import rdts.protocols.knowledgeGroups.MultiPaxos

class PaperGroups extends munit.FunSuite {
  val client: Uid    = Uid.predefined("client")
  val leader: Uid    = Uid.predefined("leader")
  val follower1: Uid = Uid.predefined("follower1")
  val follower2: Uid = Uid.predefined("follower2")
  val follower3: Uid = Uid.predefined("follower3")
  val follower4: Uid = Uid.predefined("follower4")
  val proxy1: Uid    = Uid.predefined("proxy1")
  val proxy2: Uid    = Uid.predefined("proxy2")

  test("ClientServerSystem Smoke Test") {
    val a = MultiPaxos[Request]()
    given ReplicaId(leader)
    val delta = a.startLeaderElection(0)

    assert(KnowledgeGroups.clientServer.matches(delta, follower1))
    assert(KnowledgeGroups.clientServer.matches(delta, leader))
  }

  test("test local connections with client-server knowledge groups") {
    val cluster = KnowledgeGroupFixtures.Cluster(
      KnowledgeGroups.clientServer,
      List(leader, client, follower1, follower2, follower3, follower4),
      Set(leader, follower1, follower2)
    )
    val replicas        = cluster.replicas
    def deliver(): Unit = cluster.deliver()
    def settle(): Unit  = cluster.settle()

    replicas(leader).startLeaderElection()
    deliver()
    replicas(client).request("0")
    deliver()

    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    assertEquals(replicas(leader).state.read.map(_.payload), Seq("0"))
    assertEquals(replicas(client).state.read.map(_.payload), Seq("0"))
    assertEquals(replicas(follower1).state.read.map(_.payload), Seq("0"))
    assertEquals(replicas(follower2).state.read.map(_.payload), Seq("0"))

    replicas(client).request("1")
    settle()
    assertEquals(replicas(leader).state.read.map(_.payload), Seq("0", "1"))
    assertEquals(replicas(client).state.read.map(_.payload), Seq("0", "1"))
    assertEquals(replicas(follower1).state.read.map(_.payload), Seq("0", "1"))
    assertEquals(replicas(follower2).state.read.map(_.payload), Seq("0", "1"))
    assertEquals(replicas(leader).state, replicas(follower1).state)
    assertEquals(replicas(follower2).state, replicas(follower1).state)
    // assertNotEquals(replicas(leader).state, replicas(client).state)
  }

  test("test local connections with compartmentalization") {
    val cluster = KnowledgeGroupFixtures.Cluster(
      KnowledgeGroups.compartmentalized,
      List(leader, proxy1, proxy2, client, follower1, follower2, follower3, follower4),
      Set(leader, follower1, follower2, follower3, follower4)
    )
    val replicas        = cluster.replicas
    def deliver(): Unit = cluster.deliver()
    def settle(): Unit  = cluster.settle()

    replicas(leader).startLeaderElection()
    settle()
    replicas(client).request("0")
    settle()

    // assert(!replicas(client).state.requests.elements.isEmpty)
    assert(replicas(leader).state.requests.elements.isEmpty)
    assert(!replicas(leader).state.slots.isEmpty)
    assert(replicas(follower1).state.requests.elements.isEmpty)

    // assertEquals(replicas(leader).state.read.map(_.payload), Seq("0"))
    assertEquals(replicas(client).state.read.map(_.payload), Seq("0"))

    replicas(client).request("1")
    settle()
    // assertEquals(replicas(leader).state.read.map(_.payload), Seq("0", "1"))
    assertEquals(replicas(client).state.read.map(_.payload), Seq("0", "1"))

    assertNotEquals(replicas(leader).state, replicas(client).state)
  }

}
