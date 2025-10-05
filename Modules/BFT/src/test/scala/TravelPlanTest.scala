import ex2024travel.lofi_acl.travelplanner.TravelPlan
import rdts.base.{LocalUid, Uid}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_Array, given_Xorable_Array}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}

class TravelPlanTest extends munit.FunSuite:

  test("Synchronise TravelPlan's deltas using RIBLT") {
    type Delta = TravelPlan

    // create the first replica and add merge it with 3 deltas
    // Replica1 = (delta1, delta2, delta3)
    var replica1 = TravelPlan.empty
    val delta1 = replica1.addBucketListEntry("entry 1")(using LocalUid(Uid("replica 1")))
    replica1 = replica1.merge(delta1)
    val delta2 = replica1.addBucketListEntry("entry 2")(using LocalUid(Uid("replica 1")))
    replica1 = replica1.merge(delta2)
    val delta3 = replica1.addBucketListEntry("entry 3")(using LocalUid(Uid("replica 1")))
    replica1 = replica1.merge(delta3)

    // create the second replica and merge it with 3 deltas, 2 of those deltas (delta1 & delta2) are the same deltas
    // merged with replica1 (to simulate a previous sync), the other delta (delta4) is new and exclusive to replica2 and
    // not yet merged with replica1
    // Replica2 = (delta1, delta2, delta4)
    var replica2 = TravelPlan.empty
    replica2 = replica2.merge(delta1)
    replica2 = replica2.merge(delta2)
    val delta4 = replica2.addBucketListEntry("entry 4")(using LocalUid(Uid("replica 2")))
    replica2 = replica2.merge(delta4)

    val lstDelta1 = List[Delta](delta1, delta2, delta3)  // deltas already merged to replica1
    val lstDelta2 = List[Delta](delta1, delta2, delta4)  // deltas already merged to replica2

    val ribltReplica1 = RIBLT[Array[Byte]]()            // riblt instance of replica1
    val ribltReplica2 = RIBLT[Array[Byte]]()            // riblt instance of replica2

    // add the deltas of replica1 to its riblt instance
    for (item <- lstDelta1)
      ribltReplica1.addSymbol(writeToArray(item))

    // add the deltas of replica2 to its riblt instance
    for (item <- lstDelta2)
      ribltReplica2.addSymbol(writeToArray(item))


    // to start the sync process, the 2 nodes that want to synchronise should agree on who is going to produce the
    // codedSymbols (sender) and who is going to apply these codedSymbol to its local RIBLT (receiver) and try to decode
    // it. At the end of the process only the receiver knows the answer to the question: which symbols (deltas) are
    // exclusive to which replica? and we assume that this replica (the receiver) will share this answer with the other
    // replica (the sender) by sending the deltas that this replica is missing to her
    var i = 0
    while !ribltReplica2.isDecoded do
      // as long as the riblt instance of replica2 cannot decode all the codedSymbols, send a codedSymbol from replica1
      // to repplica2
      ribltReplica2.addCodedSymbol(ribltReplica1.produceNextCodedSymbol)
      i += 1

    // once all the codedSymbols are decoded, send all the exclusive local deltas (of replica2) to replica1
    for (d <- ribltReplica2.localSymbols) {
      val delta = readFromArray[Delta](d.value)
      println(s"sending delta from replica2 to replica1: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica1 = replica1.merge(delta)
    }

    // merge all the exclusive remote deltas (of replica2) locally
    for (d <- ribltReplica2.remoteSymbols)
      val delta = readFromArray[Delta](d.value)
      println(s"merging delta from replica1 into replica2: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica2 = replica2.merge(delta)

    assertEquals(replica1, replica2)

    /*given Hashable[Delta]:
      extension (d: Delta) override def hash: Long =
        MurmurHash3.stringHash(s"${d.hashCode()}${d.bucketList.hashCode()}${d.expenses.hashCode()}")


    given Xorable[Delta]:
      extension (d1: Delta) override def xor(d2: Delta): Delta =
        TravelPlan(
          title = d1.title.xor(d2.title),
          bucketList = d1.bucketList.xor(d2.bucketList),
          expenses = d1.expenses.xor(d2.expenses)
        )

      extension (a: Delta) override def removeTrailingZeros(): Delta = a

      override def zero: Delta = TravelPlan.empty

    given Xorable[LastWriterWins[String]]:
      extension (lww1: LastWriterWins[String]) override def xor(lww2: LastWriterWins[String]): LastWriterWins[String] =
        LastWriterWins[String](
          timestamp = CausalTime(
            time = lww1.timestamp.time.xor(lww2.timestamp.time),
            causal = lww1.timestamp.causal.xor(lww2.timestamp.causal),
            random = lww1.timestamp.random.xor(lww2.timestamp.random)
          ),
          payload = lww1.payload.xor(lww2.payload)
        )

      extension (a: LastWriterWins[String]) override def removeTrailingZeros(): LastWriterWins[String] = a

      override def zero: LastWriterWins[String] = LastWriterWins.empty[String]

    given Xorable[ObserveRemoveMap[UniqueId, LastWriterWins[String]]]:*/



  }

