package probench

import probench.clients.ProBenchClient
import probench.data.{ClusterState, KVOperation}
import rdts.base.{LocalUid, Uid}
import rdts.protocols.Participants

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class ClusterConsensus extends munit.FunSuite {
  test("simple consensus") {

    val ids = Set("Node1", "Node2", "Node3").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: secondaries =
      ids.map { id =>
        KeyValueReplica(id, ids, offloadSending = false, offloadReplica = false, commitReads = true)
      }.toList: @unchecked
    val connection = channels.SynchronousLocalConnection("cluster-main")
    primary.cluster.dataManager.addServerConnection(connection.server)
    secondaries.foreach { node => node.cluster.dataManager.addClientConnection(connection.client(node.uid.toString)) }
    val connection2 = channels.SynchronousLocalConnection("cluster-secondary")
    secondaries.head.cluster.dataManager.addServerConnection(connection2.server)
    secondaries(1).cluster.dataManager.addClientConnection(connection2.client(secondaries(1).uid.toString))

    val clientConnectionWrite = channels.SynchronousLocalConnection("client-write")
    val clientConnectionRead  = channels.SynchronousLocalConnection("client-read")

    primary.client.dataManagerWrite.addServerConnection(clientConnectionWrite.server)
    primary.client.dataManagerRead.addServerConnection(clientConnectionRead.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, logTimings = false)
    client.writeDataManager.addClientConnection(clientConnectionWrite.client(clientUid.toString))
    client.readDataManager.addClientConnection(clientConnectionRead.client(clientUid.toString))

    client.printResults = false

    client.write("test", "Hi")
    client.read("test")

    nodes.foreach: v =>
        v.cluster.dataManager.repairTick()
        v.cluster.dataManager.repairTick()

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

    assertEquals(nodes(0).cluster.state.closedRounds(1)._2, KVOperation.Write("test2", "Hi"))
    assertEquals(nodes(2).cluster.state.closedRounds.size, 1)

  }

  test("consensus with one node") {

    val ids = Set("Node1").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: x =
      ids.map { id =>
        KeyValueReplica(id, ids, offloadSending = false, offloadReplica = false, commitReads = true)
      }.toList: @unchecked
    val connection = channels.SynchronousLocalConnection("single-cluster")
    primary.cluster.dataManager.addServerConnection(connection.server)

    val clientConnectionWrites = channels.SynchronousLocalConnection("single-client-write")
    val clientConnectionReads  = channels.SynchronousLocalConnection("single-client-read")

    primary.client.dataManagerWrite.addServerConnection(clientConnectionWrites.server)
    primary.client.dataManagerRead.addServerConnection(clientConnectionReads.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, logTimings = false)
    client.writeDataManager.addClientConnection(clientConnectionWrites.client(clientUid.toString))
    client.readDataManager.addClientConnection(clientConnectionReads.client(clientUid.toString))

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
    val f = Future.traverse(Range(0, 1000))(n => client.writeWithResult(n.toString, "value"))
    Await.ready(f, 5.seconds)

    assertEquals(primary.cluster.state.closedRounds.size, 1000)

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
