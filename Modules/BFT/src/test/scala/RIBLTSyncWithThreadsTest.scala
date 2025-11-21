import datatypes.{ORSet, Op}
import network.Network
import riblt.RIBLTSyncWithThreads
import riblt.RIBLTSyncWithThreads.given_JsonValueCodec_ORSet
import riblt.SessionType.{receiver, sender}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Random

class RIBLTSyncWithThreadsTest extends munit.FunSuite:
    override def munitTimeout: Duration = 5.minutes

    test("basic") {
      var crdt0 = ORSet[String]()
      crdt0 = crdt0.merge(crdt0.add("hello"))
      crdt0 = crdt0.merge(crdt0.add("hola"))
      crdt0 = crdt0.merge(crdt0.add("Gday Mate"))

      var crdt1 = ORSet[String]()
      crdt1 = crdt1.merge(crdt1.add("hi"))
      crdt1 = crdt1.merge(crdt1.add("bonjour"))
      crdt1 = crdt1.merge(crdt1.add("hallo"))

      var crdt2 = ORSet[String]()
      crdt2 = crdt2.merge(crdt2.add("Guten Tag"))
      crdt2 = crdt2.merge(crdt2.add("Ni hao"))
      crdt2 = crdt2.merge(crdt2.add("Konichiwa"))

      var crdt3 = ORSet[String]()
      crdt3 = crdt3.merge(crdt3.add("blalalala"))
      crdt3 = crdt3.merge(crdt3.add("hehehehee"))
      crdt3 = crdt3.merge(crdt3.add("hahahahah"))

      val sync0 = RIBLTSyncWithThreads(crdt0, "replica_0")
      val sync1 = RIBLTSyncWithThreads(crdt1, "replica_1")
      val sync2 = RIBLTSyncWithThreads(crdt2, "replica_2")
      val sync3 = RIBLTSyncWithThreads(crdt3, "replica_3")

      Network.startChannel("replica_0")
      Network.startChannel("replica_1")
      Network.startChannel("replica_2")
      Network.startChannel("replica_3")

      val t0 = sync0.startSession(sync1.replicaID, sessionType = sender)
      val t1 = sync1.startSession(sync0.replicaID, sessionType = receiver)

      val t2 = sync2.startSession(sync1.replicaID, sessionType = receiver)
      val t3 = sync1.startSession(sync2.replicaID, sessionType = sender)

      val t4 = sync3.startSession(sync2.replicaID, sessionType = receiver)
      val t5 = sync2.startSession(sync3.replicaID, sessionType = sender)

      t0.join()
      t1.join()
      t2.join()
      t3.join()
      t4.join()

      val crdt0afterSync = sync0.replica
      val crdt1afterSync = sync1.replica
      val crdt2afterSync = sync2.replica
      val crdt3afterSync = sync3.replica

      // sync2.startSession(sync3.id, sessionType=sender)
      // sync3.startSession(sync2.id, sessionType=receiver)

      // println(crdt1.elements.keySet)
      // println(crdt2.elements.keySet)
      // println(crdt3.elements.keySet)

      println(crdt0afterSync.elements.keySet)
      println(crdt1afterSync.elements.keySet)
      println(crdt2afterSync.elements.keySet)
      println(crdt3afterSync.elements.keySet)

      assertEquals(
        crdt1afterSync.elements.keySet,
        crdt0.elements.keySet ++ crdt1.elements.keySet ++ crdt2.elements.keySet
      )
    }

    test("basic_2") {
      var crdt0 = ORSet[String]()
      crdt0 = crdt0.merge(crdt0.add("hello"))

      var crdt1 = ORSet[String]()
      crdt1 = crdt1.merge(crdt1.add("hi"))

      val sync0 = RIBLTSyncWithThreads(crdt0, "replica_0")
      val sync1 = RIBLTSyncWithThreads(crdt1, "replica_1")

      Network.startChannel("replica_0")
      Network.startChannel("replica_1")
      Network.startChannel("replica_2")
      Network.startChannel("replica_3")

      val t0 = sync0.startSession(sync1.replicaID, sessionType = sender)
      val t1 = sync1.startSession(sync0.replicaID, sessionType = receiver)

      t0.join()
      t1.join()

      val crdt0afterSync = sync0.replica
      val crdt1afterSync = sync1.replica

      // sync2.startSession(sync3.id, sessionType=sender)
      // sync3.startSession(sync2.id, sessionType=receiver)

      // println(crdt1.elements.keySet)
      // println(crdt2.elements.keySet)
      // println(crdt3.elements.keySet)

      println(crdt0afterSync.elements.keySet)
      println(crdt1afterSync.elements.keySet)

    }

    test("basic_3") {
      Network.startChannel("replica1")
      Network.startChannel("replica2")

      var replica1 = ORSet[String]()
      var replica2 = ORSet[String]()

      for i <- 0 to 10 do
          val r = Random().nextDouble()
          if r <= 0.8 then {
            replica1 = replica1.merge(replica1.add(i.toString))
            replica2 = replica2.merge(replica2.add(i.toString))
          } else
              val rr = Random().nextDouble()
              if rr <= 0.5 then
                  replica1 = replica1.merge(replica1.add(i.toString))
              else
                  replica2 = replica2.merge(replica2.add(i.toString))

      val riblt1 = RIBLTSyncWithThreads(replica1, "replica1")
      val riblt2 = RIBLTSyncWithThreads(replica2, "replica2")

      val t1 = riblt1.startSession("replica2", sender)
      val t2 = riblt2.startSession("replica1", receiver)

      t1.join()
      t2.join()

      println(replica1.getElements)
      println(replica2.getElements)

      println(riblt1.replica.getElements)
      println(riblt2.replica.getElements)

    }
