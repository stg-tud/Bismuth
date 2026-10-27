package test.rdts.protocols.knowledgeGroups
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import probench.{KnowledgeGroups, MultiPaxosReplica, Request}
import rdts.base.{ReplicaId, Uid}
import rdts.protocols.knowledgeGroups.MultiPaxos
import rdts.protocols.Participants

class PaperGroups extends munit.FunSuite {
  val client    = Uid.predefined("client")
  val leader    = Uid.predefined("leader")
  val follower1 = Uid.predefined("follower1")
  val follower2 = Uid.predefined("follower2")
  val follower3 = Uid.predefined("follower3")
  val follower4 = Uid.predefined("follower4")
  val proxy1    = Uid.predefined("proxy1")
  val proxy2    = Uid.predefined("proxy2")

  test("ClientServerSystem Smoke Test") {
    val a = MultiPaxos[Request]()
    given Participants(Set(leader, follower1, follower2))
    given ReplicaId(leader)
    val delta = a.startLeaderElection(0)

    assert(KnowledgeGroups.clientServer.matches(delta, follower1))
    assert(KnowledgeGroups.clientServer.matches(delta, leader))
  }

  test("test local connections with client-server knowledge groups".ignore) {
    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids = List(leader, client, follower1, follower2, follower3, follower4)
    given Participants(Set(leader, follower1, follower2, follower3, follower4))

    val systemConfig = KnowledgeGroups.clientServer

    val replicas: Map[Uid, MultiPaxosReplica] = ids.map(f =
      i =>
        (
          i,
          MultiPaxosReplica(
            id = i,
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
            remaining.toList match {
              case primary :: Nil         => ()
              case primary :: secondaries =>
                println(s"Setting up connection for $allIds with $primary as server and $secondaries as clients")
                val connection = channels.connection.SynchronousLocalConnection(primary.toString)
                replicas(primary).dataManagers(allIds).addServerConnection(connection.server)
                secondaries.foreach(id =>
                  replicas(id).dataManagers(allIds).addClientConnection(connection.client(id.toString))
                )
                _setupConnections(secondaries.toSet)
              case Nil => ()
            }
        _setupConnections(allIds)

    systemConfig.knowledgeGroups.map(_.ids).foreach(setupConnections)
    def settle(): Unit =
      for _ <- 1 to 5 do replicas.values.foreach(_.dataManagers.values.foreach(_.tick()))

    settle()
    assertEquals(replicas(leader).dataManagers.size, 2)
    assertEquals(replicas(follower1).dataManagers.size, 1)

    replicas(leader).startLeaderElection()
    replicas(client).request("0")

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
    given JsonValueCodec[MultiPaxos[Int]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids =
      List(leader, proxy1, proxy2, client, follower1, follower2, follower3, follower4)
    given Participants(Set(leader, follower1, follower2, follower3, follower4))

    val systemConfig = KnowledgeGroups.compartmentalized

    val replicas: Map[Uid, MultiPaxosReplica] = ids.map(f =
      i =>
        (
          i,
          MultiPaxosReplica(
            id = i,
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
            remaining.toList match {
              case primary :: Nil         => ()
              case primary :: secondaries =>
                println(s"Setting up connection for $allIds with $primary as server and $secondaries as clients")
                val connection = channels.connection.SynchronousLocalConnection(primary.toString)
                replicas(primary).dataManagers(allIds).addServerConnection(connection.server)
                secondaries.foreach(id =>
                  replicas(id).dataManagers(allIds).addClientConnection(connection.client(id.toString))
                )
                _setupConnections(secondaries.toSet)
              case Nil => ()
            }
        _setupConnections(allIds)

    systemConfig.knowledgeGroups.map(_.ids).foreach(setupConnections)
    def settle(): Unit =
      for _ <- 1 to 5 do replicas.values.foreach(_.dataManagers.values.foreach(_.tick()))

    settle()
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
