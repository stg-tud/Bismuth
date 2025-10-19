import bfttravelplanner.BFTTravelPlan
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import rdts.base.{LocalUid, Uid}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_Array, given_Xorable_Array}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}

import scala.util.Random

class TravelPlanTest extends munit.FunSuite:

  type Delta = TravelPlan
  private val replica1Uid = LocalUid(Uid("replica 1"))
  private val replica2Uid = LocalUid(Uid("replica 2"))
  private val replica3Uid = LocalUid(Uid("replica 3"))
  private val testSetSize = if isCI then 500 else 10_000

  test("Example 1: Synchronise TravelPlan's deltas using RIBLT") {

    // create the first replica and add merge it with 3 deltas
    // Replica1 = (delta1, delta2, delta3)
    var replica1 = TravelPlan.empty
    val delta1   = replica1.addBucketListEntry("entry 1")(using replica1Uid)
    replica1 = replica1.merge(delta1)
    val delta2 = replica1.addBucketListEntry("entry 2")(using replica1Uid)
    replica1 = replica1.merge(delta2)
    val delta3 = replica1.addBucketListEntry("entry 3")(using replica1Uid)
    replica1 = replica1.merge(delta3)

    // create the second replica and merge it with 3 deltas, 2 of those deltas (delta1 & delta2) are the same deltas
    // merged with replica1 (to simulate a previous sync), the other delta (delta4) is new and exclusive to replica2 and
    // not yet merged with replica1
    // Replica2 = (delta1, delta2, delta4)
    var replica2 = TravelPlan.empty
    replica2 = replica2.merge(delta1)
    replica2 = replica2.merge(delta2)
    val delta4 = replica2.addBucketListEntry("entry 4")(using replica2Uid)
    replica2 = replica2.merge(delta4)

    val lstDelta1 = List[Delta](delta1, delta2, delta3) // deltas already merged to replica1
    val lstDelta2 = List[Delta](delta1, delta2, delta4) // deltas already merged to replica2

    val ribltReplica1 = RIBLT[Array[Byte]]() // riblt instance of replica1
    val ribltReplica2 = RIBLT[Array[Byte]]() // riblt instance of replica2

    // add the deltas of replica1 to its riblt instance
    for item <- lstDelta1 do
      ribltReplica1.addSymbol(writeToArray(item))

    // add the deltas of replica2 to its riblt instance
    for item <- lstDelta2 do
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
    for d <- ribltReplica2.localSymbols do {
      val delta = readFromArray[Delta](d.value)
      // println(s"sending delta from replica2 to replica1: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica1 = replica1.merge(delta)
    }

    // merge all the exclusive remote deltas (of replica2) locally
    for d <- ribltReplica2.remoteSymbols do
      val delta = readFromArray[Delta](d.value)
      // println(s"merging delta from replica1 into replica2: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica2 = replica2.merge(delta)

    assertEquals(replica1, replica2)

  }

  test("Example 2: Synchronise TravelPlan's deltas using RIBLT") {

    /** Same as the above test, only with a lot more deltas */

    var replica1  = TravelPlan.empty
    var replica2  = TravelPlan.empty
    var lstDelta1 = List[Delta]()
    var lstDelta2 = List[Delta]()

    var j = 0
    for i <- 0 to testSetSize do
      val r = Random().nextDouble()
      if r <= 0.8 then {
        val d = TravelPlan.empty.addBucketListEntry(i.toString)(using if r <= 0.4 then replica1Uid else replica2Uid)
        lstDelta1 = lstDelta1 :+ d
        lstDelta2 = lstDelta2 :+ d
        replica1 = replica1.merge(d)
        replica2 = replica2.merge(d)
      } else {
        j += 1
        val d = TravelPlan.empty.addBucketListEntry(i.toString)(using replica3Uid)
        if r <= 0.9 then {
          lstDelta1 = lstDelta1 :+ d
          replica1 = replica1.merge(d)
        } else {
          lstDelta2 = lstDelta2 :+ d
          replica2 = replica2.merge(d)
        }

      }

    val ribltReplica1 = RIBLT[Array[Byte]]()
    val ribltReplica2 = RIBLT[Array[Byte]]()

    for item <- lstDelta1 do
      ribltReplica1.addSymbol(writeToArray(item))

    for item <- lstDelta2 do
      ribltReplica2.addSymbol(writeToArray(item))

    var i = 0
    while !ribltReplica2.isDecoded do
      ribltReplica2.addCodedSymbol(ribltReplica1.produceNextCodedSymbol)
      i += 1

    for d <- ribltReplica2.localSymbols do {
      val delta = readFromArray[Delta](d.value)
      // println(s"sending delta from replica2 to replica1: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica1 = replica1.merge(delta)
    }

    for d <- ribltReplica2.remoteSymbols do
      val delta = readFromArray[Delta](d.value)
      // println(s"merging delta from replica1 into replica2: ${delta.bucketList.queryAllEntries.map(e => e.payload)}")
      replica2 = replica2.merge(delta)

    assertEquals(replica1, replica2)

  }

  test("TravelPlan + HashDAG + RIBLT") {
    var r1 = BFTTravelPlan()
    r1 = r1.merge(r1.addBucketListEntry("entry 1")(using replica1Uid))
    r1 = r1.merge(r1.addBucketListEntry("entry 11")(using replica1Uid))

    // println(r1.state.bucketList.queryAllEntries)

    var r2 = BFTTravelPlan()
    r2 = r2.merge(r2.addBucketListEntry("entry 2")(using replica2Uid))
    r2 = r2.merge(r2.addBucketListEntry("entry 22")(using replica2Uid))

    // println(r2.state.bucketList.queryAllEntries)

    while !r1.riblt.isDecoded do
      r1.addCodedSymbols(r2.produceNextCodedSymbols())

    val syncReq = r1.sendSyncRequest
    r2 = r2.merge(syncReq.delta)
    r1 = r1.merge(r2.sendSyncResponse(syncReq.requestedEvents))

    r1 = r1.processQueue
    r2 = r2.processQueue

    // println(r1.state.bucketList.queryAllEntries.map(lww => lww.payload))
    // println(r2.state.bucketList.queryAllEntries.map(lww => lww.payload))

    assertEquals(
      r1.state.bucketList.queryAllEntries.map(lww => lww.payload).toSet,
      r2.state.bucketList.queryAllEntries.map(lww => lww.payload).toSet
    )

  }
