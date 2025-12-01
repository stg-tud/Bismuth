package benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import dag.Event
import datatypes.{Counter, ORSet, Op, Replica}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_JsonValueCodec_CodedSymbol, given_Xorable_String}
import java.security.{PrivateKey, PublicKey}

given JsonValueCodec[ORSet[String]] = JsonCodecMaker.make
given JsonValueCodec[Counter] = JsonCodecMaker.make
given JsonValueCodec[PublicKey]     = new JsonValueCodec[PublicKey] {
  override def encodeValue(key: PublicKey, out: JsonWriter): Unit =
    out.writeBase64Val(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(key).getBytes, false)

  override def decodeValue(in: JsonReader, default: PublicKey): PublicKey =
    Ed25519Util.base64PublicKeyBytesToPublicKey(String(in.readBase64AsBytes(Array.empty[Byte])))

  override def nullValue: PublicKey = null.asInstanceOf[PublicKey]
}

given JsonValueCodec[PrivateKey] = new JsonValueCodec[PrivateKey] {
  override def encodeValue(key: PrivateKey, out: JsonWriter): Unit =
    out.writeRawVal(Ed25519Util.privateKeyToRawPrivateKeyBytes(key))

  override def decodeValue(in: JsonReader, default: PrivateKey): PrivateKey =
    Ed25519Util.rawPrivateKeyBytesToPrivateKey(in.readRawValAsBytes())

  override def nullValue: PrivateKey = null.asInstanceOf[PrivateKey]
}

object SyncStrategies {

  def syncNaively[T, R <: Replica[T, R]](replica1: Replica[T, R], replica2: Replica[T, R]): (Int, Int) =
      var roundTrips = 0
      var bandwidth  = 0

      // Step 1: Exchange Heads of the HashDAG
      val heads1 = replica1.hashDAG.getCurrentHeadsIDs
      val heads2 = replica2.hashDAG.getCurrentHeadsIDs
      roundTrips += 1
      bandwidth += (heads1.size + heads2.size)

      // Step 2: If the heads are different, sync
      if heads1 != heads2 then
          val exclusiveHeads1 = heads1 -- heads2
          val exclusiveHeads2 = heads2 -- heads1
          val mutualHeads     = heads1.intersect(heads2)
          val IDs1            = replica1.hashDAG.getIDs
          val IDs2            = replica2.hashDAG.getIDs
          val exclusiveIDs1   =
            IDs1.filter(id => exclusiveHeads1.exists(head => replica1.hashDAG.pathExists(id, head)))
          val exclusiveIDs2 =
            IDs2.filter(id => exclusiveHeads2.exists(head => replica2.hashDAG.pathExists(id, head)))

          roundTrips += 1
          bandwidth += (exclusiveIDs1.size + exclusiveIDs2.size)

          assert((IDs1 ++ exclusiveIDs2) == (IDs2 ++ exclusiveIDs1))

      (roundTrips, bandwidth)

  def syncPingPong[T, R <: Replica[T, R]](replica1: Replica[T, R], replica2: Replica[T, R])(using JsonValueCodec[Event[T]]): (Int, Int) = {
    var roundTrips = 0
    var bandwidth  = 0

    // Step 1: Exchange Heads of the HashDAG
    val replica1IDs = replica1.hashDAG.getIDs
    val replica1Events = replica1.hashDAG.events.values.toSet
    val replica2IDs = replica2.hashDAG.getIDs
    val replica2Events = replica2.hashDAG.events.values.toSet
    var messagesForReplica1 = replica2.hashDAG.getCurrentHeadsIDs.map(id => replica2.hashDAG.events(id))
    var receivedReplica1 = List.empty[Event[T]]
    var messagesForReplica2 = replica1.hashDAG.getCurrentHeadsIDs.map(id => replica1.hashDAG.events(id))
    var receivedReplica2 = List.empty[Event[T]]
    var missingReplica1 = Set.empty[String]
    var missingReplica2 = Set.empty[String]

    while messagesForReplica1.nonEmpty || messagesForReplica2.nonEmpty || missingReplica1.nonEmpty || missingReplica2.nonEmpty do
      // replica 1
      for event <- messagesForReplica1 do
        if !replica1Events.contains(event) then {
          receivedReplica1 = receivedReplica1 :+ event
          for parent <- event.dependencies do
            if !replica1IDs.contains(parent) then
              missingReplica1 = missingReplica1 + parent
        }
      bandwidth = messagesForReplica1.foldLeft(bandwidth)((b, e) => b + writeToArray(e).length)
      messagesForReplica1 = Set.empty[Event[T]]

      for id <- missingReplica2 do
        messagesForReplica2 = messagesForReplica2 + replica1.hashDAG.events(id)

      bandwidth = missingReplica2.foldLeft(bandwidth)((b, e) => b + e.getBytes("UTF-8").length)
      missingReplica2 = Set.empty[String]

      // replica 2
      for event <- messagesForReplica2 do
        if !replica2Events.contains(event) then {
          receivedReplica2 = receivedReplica2 :+ event
          for parent <- event.dependencies do
            if !replica2IDs.contains(parent) then
              missingReplica2 = missingReplica2 + parent
        }
      bandwidth = messagesForReplica2.foldLeft(bandwidth)((b, e) => b + writeToArray(e).length)
      messagesForReplica2 = Set.empty[Event[T]]

      for id <- missingReplica1 do
        messagesForReplica1 = messagesForReplica1 + replica2.hashDAG.events(id)

      bandwidth = missingReplica2.foldLeft(bandwidth)((b, e) => b + e.getBytes("UTF-8").length)
      missingReplica1 = Set.empty[String]

      roundTrips += 1

    assert(replica2IDs -- replica1IDs == receivedReplica1.map(e => e.id).toSet)
    assert(replica1IDs -- replica2IDs == receivedReplica2.map(e => e.id).toSet)

    println(s"SyncPingPong: diff = ${(replica1IDs -- replica2IDs).size}, total = ${replica1IDs.size}, ${replica2IDs.size}")
    println(s"roundTrips = $roundTrips, bandwidth = $bandwidth")

    (roundTrips, bandwidth)

  }

  def syncPingPongThreaded[T, R <: Replica[T, R]](replica1: R, replica2: R)(using JsonValueCodec[R]): (Int, Int) = {
    val stats = SyncStats()

    val t1 = new PingPongSync(replica1, "replica1", "replica2", stats)
    val t2 = new PingPongSync(replica2, "replica2", "replica1", stats)

    t1.start()
    t2.start()

    t1.join()
    t2.join()

    stats.results
  }

  def syncRIBLT[T, R <: Replica[T, R]](replica1: R, replica2: R, codedSymbolsPerRoundTrip: Int = 10)(using JsonValueCodec[Event[T]]): (Int, Int) = {
    val riblt1     = RIBLT[String]()
    val riblt2     = RIBLT[String]()
    var roundTrips = 0
    var bandwidth = 0

    for id <- replica1.hashDAG.getIDs do
        riblt1.addSymbol(id)

    for id <- replica2.hashDAG.getIDs do
        riblt2.addSymbol(id)

    while !riblt1.isDecoded do {
      val cs = riblt2.produceNextCodedSymbolsAsBytes(codedSymbolsPerRoundTrip)
      riblt1.addCodedSymbolsAsBytes(cs)
      roundTrips += 1
      bandwidth += cs.length
      //println(roundTrips)
    }

    val localEvents = riblt1.localSymbols.map(s => replica1.hashDAG.events(s.value))
    val remoteEvents = riblt1.remoteSymbols.map(s => replica2.hashDAG.events(s.value))

    bandwidth = (localEvents ++ remoteEvents).foldLeft(bandwidth)((b, e) => b + writeToArray(e).length)

    val replica1IDs = replica1.hashDAG.getIDs
    val replica2IDs = replica2.hashDAG.getIDs

    println(s"SyncRIBLT: diff = ${(replica1IDs -- replica2IDs).size}, total = ${replica1IDs.size}, ${replica2IDs.size}")
    println(s"roundTrips = $roundTrips, bandwidth = $bandwidth")
    println(s"coded symbols per roundtrip = $codedSymbolsPerRoundTrip")

    (roundTrips, bandwidth)
  }

  @main def main(): Unit =
    given c1: JsonValueCodec[Event[Op[String]]] = JsonCodecMaker.make
    given c2: JsonValueCodec[Event[Int]] = JsonCodecMaker.make

    val replica1 = ReplicaExamples.Example1.replica1
    val replica2 = ReplicaExamples.Example1.replica2

    //println(syncNaively(replica1, replica2))
    println(syncPingPong(replica1, replica2))
    println(syncRIBLT(replica1, replica2))
    //val res = syncPingPongThreaded(replica1, replica2)
    //println(s"(${res._1 / 2}, ${res._2})")

    println("------")
    val replica3 = ReplicaExamples.Example2.replica1
    val replica4 = ReplicaExamples.Example2.replica2
    syncPingPong(replica3, replica4)
    syncRIBLT(replica3, replica4)
    //val res1 = syncPingPongThreaded(replica3, replica4)
    //println(s"(${res1._1 / 2}, ${res1._2})")

    println("------")
    val replica7 = ReplicaExamples.Example3.replica1
    val replica8 = ReplicaExamples.Example3.replica2
    syncPingPong(replica7, replica8)
    syncRIBLT(replica7, replica8)

    println("------")
    val replica5 = ReplicaExamples.Example4.replica1
    val replica6 = ReplicaExamples.Example4.replica2
    syncPingPong(replica5, replica6)
    syncRIBLT(replica5, replica6): Unit
    //val res2 = syncPingPongThreaded(replica5, replica6)
    //println(s"(${res2._1 / 2}, ${res2._2})")
}

/*

SyncPingPong: diff = 5, total = 10, 7
roundTrips = 3, bandwidth = 10076
(3,10076)
SyncRIBLT: diff = 5, total = 10, 7
roundTrips = 6, bandwidth = 7298
(6,7298)
------
SyncPingPong: diff = 1005, total = 1010, 1007
roundTrips = 1003, bandwidth = 2603923
SyncRIBLT: diff = 1005, total = 1010, 1007
roundTrips = 1469, bandwidth = 2005407
------
SyncPingPong: diff = 100, total = 512, 512
roundTrips = 100, bandwidth = 259725
SyncRIBLT: diff = 100, total = 512, 512
roundTrips = 155, bandwidth = 200467
------
SyncPingPong: diff = 500, total = 1501, 1501
roundTrips = 500, bandwidth = 1265325
SyncRIBLT: diff = 500, total = 1501, 1501
roundTrips = 753, bandwidth = 967963

*/
