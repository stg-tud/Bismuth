package ex2026keyValuePrdtBench

import channels.DeltaStorage
import channels.connection.{LocalMessageQueue, QueuedLocalConnection}
import ex2026keyValuePrdtBench.clients.ProBenchClient
import ex2026keyValuePrdtBench.data.ClusterState
import rdts.base.Uid

import scala.collection.mutable

/** A replica of the key-value store that joins late applies none of the earlier writes to its key-value
  * cache.
  *
  * Known bug, the property tests are marked `.fail`. A replica applies writes to its cache in
  * `Cluster.handleIncoming` only when an incoming delta raises its `commitIndex`, and then only the
  * entries that `readSince(old.commitIndex + 1)` returns. The delta of `ParallelMultiPaxos.upkeep`
  * carries the absolute `commitIndex` of the sender but only the log entries that this call logged, and
  * `readSince(t)` reads the slots `t until log.size`, which assumes that the log is a prefix.
  *
  * A replica that connects late gets the history of its neighbor through a plumtree `Graft`. With
  * `DeltaStorage.Type.KeepAll` the history is replayed newest first. The first replayed delta raises the
  * `commitIndex` of the new replica to 2 and carries only the entry of slot 2, so `readSince(0)` returns
  * nothing. The older deltas fill the log but do not raise the `commitIndex`, so the replica never
  * applies `k1`, `k2`, `k3`. Its log ends complete, its cache empty.
  *
  * With `DeltaStorage.Type.Discarding(4)` the history keeps only the last 4 deltas, and the new replica
  * ends with `commitIndex` 2 and the log `1, 2`: the entry of slot 0 is never sent to it again.
  *
  * Trace: Node1 is elected and Node2 votes; Node3 is not connected. A client writes `k1`, `k2`, `k3`
  * through Node1. Then Node3 connects to Node1 and the repair timers fire.
  */
class KeyValueReplicaLateJoinTest extends munit.FunSuite {

  case class Result(nodes: List[KeyValueReplica], historyOfNode1: List[ClusterState])

  def kvCache(r: KeyValueReplica): Map[String, String] =
      val field = classOf[KeyValueReplica].getDeclaredFields.find(_.getName.contains("kvCache")).get
      field.setAccessible(true)
      val cache = field.get(r).asInstanceOf[mutable.HashMap[String, String]]
      cache.synchronized(cache.toMap)

  def runTrace(storage: DeltaStorage.Type): Result = {
    val queue           = LocalMessageQueue()
    def deliver(): Unit = while queue.nonEmpty do queue.deliverAll(): Unit

    val ids                   = List("Node1", "Node2", "Node3").map(Uid.predefined)
    val nodes @ List(n1, n2, n3) =
      ids.map(id => KeyValueReplica(id, ids.toSet, offloadReplica = false, deltaStorageType = storage, commitReads = true))
    val cluster = QueuedLocalConnection("late-join-cluster", queue)
    n1.cluster.dataManager.addServerConnection(cluster.server)
    n2.cluster.dataManager.addClientConnection(cluster.client("Node2"))

    val clientWrite = QueuedLocalConnection("late-join-client-write", queue)
    val clientRead  = QueuedLocalConnection("late-join-client-read", queue)
    n1.client.dataManagerWrite.addServerConnection(clientWrite.server)
    n1.client.dataManagerRead.addServerConnection(clientRead.server)
    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, logTimings = false)
    client.writeDataManager.addClientConnection(clientWrite.client(clientUid.toString))
    client.readDataManager.addClientConnection(clientRead.client(clientUid.toString))
    client.printResults = false
    deliver()

    for i <- 1 to 3 do
        client.write(s"k$i", s"v$i")
        deliver()
    val history = n1.cluster.dataManager.allPayloads.map(_.data)

    // Node3 joins late
    n3.cluster.dataManager.addClientConnection(cluster.client("Node3"))
    deliver()
    for node <- nodes; _ <- 1 to 2 do
        node.cluster.dataManager.timers.asInstanceOf[channels.Timers.Manual].fireAll()
        deliver()
    Result(nodes, history)
  }

  lazy val keepAll: Result    = runTrace(DeltaStorage.Type.KeepAll)
  lazy val discarding: Result = runTrace(DeltaStorage.Type.Discarding(4))

  val written: Map[String, String] = Map("k1" -> "v1", "k2" -> "v2", "k3" -> "v3")

  test("trace precondition: Node1 and Node2 apply k1, k2, k3") {
    for node <- keepAll.nodes.take(2) do assertEquals(kvCache(node), written, node.uid)
  }

  test("trace precondition: KeepAll replays first a delta with commitIndex 2 and only slot 2 logged") {
    val first = keepAll.historyOfNode1.head
    assertEquals(first.commitIndex, 2L)
    assertEquals(first.log.keySet, Set(2L))
  }

  test("trace precondition: with KeepAll Node3 ends with the complete log") {
    val n3 = keepAll.nodes(2).cluster.state
    assertEquals(n3.commitIndex, 2L)
    assertEquals(n3.log.keySet, Set(0L, 1L, 2L))
  }

  test("trace precondition: with Discarding(4) Node3 ends with commitIndex 2 and the log 1, 2") {
    val n3 = discarding.nodes(2).cluster.state
    assertEquals(n3.commitIndex, 2L)
    assertEquals(n3.log.keySet, Set(1L, 2L))
  }

  test("a replica that joins late applies all committed writes (KeepAll)".fail) {
    assertEquals(kvCache(keepAll.nodes(2)), written)
  }

  test("a replica that joins late applies all committed writes (Discarding(4))".fail) {
    assertEquals(kvCache(discarding.nodes(2)), written)
  }

  test("a replica that joins late logs every slot up to its commitIndex (Discarding(4))".fail) {
    val n3 = discarding.nodes(2).cluster.state
    assertEquals(n3.log.keySet, (0L to n3.commitIndex).toSet)
  }
}
