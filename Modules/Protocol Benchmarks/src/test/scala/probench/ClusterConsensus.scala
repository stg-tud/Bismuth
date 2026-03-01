package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import probench.clients.ProBenchClient
import probench.data.{ClientState, ClusterState, KVOperation}
import rdts.base.{LocalUid, Uid}
import rdts.protocols.Participants
import replication.ProtocolMessage

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}

class ClusterConsensus extends munit.FunSuite {
  test("simple consensus") {

    given JsonValueCodec[ClusterState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clusterCodec: JsonValueCodec[ProtocolMessage[ClusterState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given JsonValueCodec[ClientState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clientCodec: JsonValueCodec[ProtocolMessage[ClientState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids = Set("Node1", "Node2", "Node3").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: secondaries =
      ids.map { id =>
        KeyValueReplica(id, ids, offloadSending = false, offloadReplica = false, commitReads = true)
      }.toList: @unchecked
    val connection = channels.SynchronousLocalConnection[ProtocolMessage[ClusterState]]()
    primary.cluster.dataManager.addObjectConnection(connection.server)
    secondaries.foreach { node => node.cluster.dataManager.addObjectConnection(connection.client(node.uid.toString)) }
    val connection2 = channels.SynchronousLocalConnection[ProtocolMessage[ClusterState]]()
    secondaries.head.cluster.dataManager.addObjectConnection(connection2.server)
    secondaries(1).cluster.dataManager.addObjectConnection(connection2.client(secondaries(1).uid.toString))

    val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[ClientState]]()

    primary.client.dataManagerWrite.addObjectConnection(clientConnection.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, blocking = true, logTimings = false)
    client.writeDataManager.addObjectConnection(clientConnection.client(clientUid.toString))

    client.printResults = false

    client.write("test", "Hi")
    client.read("test")

    assertEquals(nodes(0).cluster.state, nodes(1).cluster.state)
    assertEquals(nodes(1).cluster.state, nodes(2).cluster.state)
    assertEquals(nodes(2).cluster.state, nodes(0).cluster.state)

    def investigateUpkeep(state: ClusterState)(using LocalUid) = {
      val delta  = state.upkeep
      val merged = state `merge` delta
      assert(state != merged)
      assert(delta `inflates` state, delta)
    }

    def runUpkeep() = while {
      nodes.filter(_.cluster.needsUpkeep()).exists { n =>
        investigateUpkeep(n.cluster.state)(using n.localUid)
        n.cluster.forceUpkeep()
        true
      }
    } do ()

    runUpkeep()

    nodes.foreach(node => assert(!node.cluster.needsUpkeep(), node.uid))

    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
      val current = keyValueReplica.cluster.state
      assertEquals(
        current `merge` current.upkeep(using keyValueReplica.localUid),
        current,
        s"${keyValueReplica.uid} upkeep"
      )
    }

    nodes.foreach(noUpkeep)

    assertEquals(nodes(0).cluster.state, nodes(1).cluster.state)
    assertEquals(nodes(1).cluster.state, nodes(2).cluster.state)
    assertEquals(nodes(2).cluster.state, nodes(0).cluster.state)

    // simulate crash

    secondaries.last.cluster.dataManager.globalAbort.closeRequest = true

    client.printResults = false

    client.write("test2", "Hi")
    client.read("test2")

    runUpkeep()

    nodes.foreach(noUpkeep)

    assertEquals(nodes(0).cluster.state.closedRounds(3).value, KVOperation.Read("test2"))
    assertEquals(nodes(2).cluster.state.closedRounds.size, 2)

  }

  test("consensus with one node") {

    given JsonValueCodec[ClusterState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clusterCodec: JsonValueCodec[ProtocolMessage[ClusterState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given JsonValueCodec[ClientState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clientCodec: JsonValueCodec[ProtocolMessage[ClientState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids = Set("Node1").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: x =
      ids.map { id =>
        KeyValueReplica(id, ids, offloadSending = false, offloadReplica = false, commitReads = true)
      }.toList: @unchecked
    val connection = channels.SynchronousLocalConnection[ProtocolMessage[ClusterState]]()
    primary.cluster.dataManager.addObjectConnection(connection.server)

    val clientConnectionWrites = channels.SynchronousLocalConnection[ProtocolMessage[ClientState]]()
    val clientConnectionReads  = channels.SynchronousLocalConnection[ProtocolMessage[ClientState]]()

    primary.client.dataManagerWrite.addObjectConnection(clientConnectionWrites.server)
    primary.client.dataManagerRead.addObjectConnection(clientConnectionReads.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, blocking = true, logTimings = false)
    client.writeDataManager.addObjectConnection(clientConnectionWrites.client(clientUid.toString))
    client.readDataManager.addObjectConnection(clientConnectionReads.client(clientUid.toString))

    client.printResults = false

//    Await.ready(
//      for
//          _ <- client.writeWithResult("test", "Hi")
//          _ <- client.writeWithResult("test", "Hi")
//          _ <- client.readWithResult("test")
//      yield (),
//      5.seconds
//    )

//    assertEquals(primary.client.writeQueue.requestsSorted, List.empty)
//    assertEquals(primary.cluster.state.closedRounds.size, 3)

//    for n <- Range(0,100) do {
//      Await.ready(client.writeWithResult(n.toString, "value"), 5.seconds)
//    }
    val f = Future.traverse(Range(0,100))(n => client.writeWithResult(n.toString, "value"))
    Await.ready(f, 5.seconds)

    assertEquals(primary.cluster.state.closedRounds.size, 100)

    def investigateUpkeep(state: ClusterState)(using LocalUid) = {
      val delta  = state.upkeep
      val merged = state `merge` delta
      assert(state != merged)
      assert(delta `inflates` state, delta)
    }

    def runUpkeep() = while {
      nodes.filter(_.cluster.needsUpkeep()).exists { n =>
        investigateUpkeep(n.cluster.state)(using n.localUid)
        n.cluster.forceUpkeep()
        true
      }
    } do ()

    runUpkeep()

    nodes.foreach(node => assert(!node.cluster.needsUpkeep(), node.uid))

    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
      val current = keyValueReplica.cluster.state
      assertEquals(
        current `merge` current.upkeep(using keyValueReplica.localUid),
        current,
        s"${keyValueReplica.uid} upkeep"
      )
    }

    nodes.foreach(noUpkeep)

    // simulate crash

    client.printResults = false

    client.write("test2", "Hi")
    client.read("test2")

    runUpkeep()

    nodes.foreach(noUpkeep)

  }
}
