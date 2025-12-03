/*import akka.actor.testkit.typed.scaladsl.ActorTestKit
import munit.FunSuite
import datatypes.ORSet
import riblt.RIBLTSyncAkka
import riblt.RIBLTSyncAkka.SessionType.*

class RIBLTSyncAkkaTest extends FunSuite {

  val testKit = ActorTestKit()

  test("replicas should sync") {
    var set1 = ORSet[String]()
    set1 = set1.merge(set1.add("replica_0"))

    var set2 = ORSet[String]()
    set2 = set2.merge(set2.add("replica_1"))

    var set3 = ORSet[String]()
    set3 = set3.merge(set3.add("replica_2"))

    val replica0 = testKit.spawn(RIBLTSyncAkka("replica_0", set1), "replica_0")
    val replica1 = testKit.spawn(RIBLTSyncAkka("replica_1", set2), "replica_1")
    val replica2 = testKit.spawn(RIBLTSyncAkka("replica_2", set3), "replica_2")

    // Start sync sessions
    replica0 ! RIBLTSyncAkka.StartSession(replica1, sender)
    replica1 ! RIBLTSyncAkka.StartSession(replica0, receiver)

    replica1 ! RIBLTSyncAkka.StartSession(replica2, sender)
    replica2 ! RIBLTSyncAkka.StartSession(replica1, receiver)

    val probe = testKit.createTestProbe[RIBLTSyncAkka.ReplicaResponse]()

    // wait for the asserts to be true
    probe.awaitAssert {
      replica0 ! RIBLTSyncAkka.GetReplica(probe.ref)
      val r0 = probe.receiveMessage().replica.asInstanceOf[ORSet[String]]
      Thread.sleep(100)

      replica1 ! RIBLTSyncAkka.GetReplica(probe.ref)
      val r1 = probe.receiveMessage().replica.asInstanceOf[ORSet[String]]
      Thread.sleep(100)

      replica2 ! RIBLTSyncAkka.GetReplica(probe.ref)
      val r2 = probe.receiveMessage().replica.asInstanceOf[ORSet[String]]

      // println("CRDT0 elements after sync: " + r0.elements.keySet)
      // println("CRDT1 elements after sync: " + r1.elements.keySet)
      // println("CRDT2 elements after sync: " + r2.elements.keySet)

      // assert(r0.getElements.subsetOf(r1.getElements))
      // assert(r2.getElements.subsetOf(r1.getElements))
    }
  }
}*/
