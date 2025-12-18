package benchmarks

import bloomfilter.mutable.BloomFilter
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import dag.Event
import datatypes.{Counter, ORSet, Op, Replica}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_JsonValueCodec_CodedSymbol, given_Xorable_String}

import java.io.{ByteArrayOutputStream, OutputStream}
import java.util.Base64
import java.security.{PrivateKey, PublicKey}

given c1: JsonValueCodec[Event[Int]]         = JsonCodecMaker.make
given c22: JsonValueCodec[Event[Op[String]]] = JsonCodecMaker.make
given c3: JsonValueCodec[Op[String]]         = JsonCodecMaker.make
given c4: JsonValueCodec[ORSet[String]]      = JsonCodecMaker.make
given c5: JsonValueCodec[Counter]            = JsonCodecMaker.make
given c6: JsonKeyCodec[Op[String]]           = new JsonKeyCodec[Op[String]] {
  override def decodeKey(in: JsonReader): Op[String] =
    readFromArray[Op[String]](in.readRawValAsBytes())

  override def encodeKey(x: Op[String], out: JsonWriter): Unit =
    out.writeRawVal(writeToArray(x))
}

given c7: JsonKeyCodec[Event[Op[String]]] = new JsonKeyCodec[Event[Op[String]]] {
  override def decodeKey(in: JsonReader): Event[Op[String]] =
    readFromArray[Event[Op[String]]](in.readRawValAsBytes())

  override def encodeKey(x: Event[Op[String]], out: JsonWriter): Unit =
    out.writeRawVal(writeToArray(x))
}

given c8: JsonKeyCodec[Event[Int]] = new JsonKeyCodec[Event[Int]] {
  override def decodeKey(in: JsonReader): Event[Int] =
    readFromArray[Event[Int]](in.readRawValAsBytes())

  override def encodeKey(x: Event[Int], out: JsonWriter): Unit =
    out.writeRawVal(writeToArray(x))
}

given JsonValueCodec[PublicKey] = new JsonValueCodec[PublicKey] {
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

  def rsync[T, R <: Replica[T, R]](
      replica1: Replica[T, R],
      replica2: Replica[T, R],
      size: Int,
      diff: Float,
      dependencyPerRoundTrip: Int = 1,
      deltaSize: Int,
  )(using JsonValueCodec[Event[T]]): Measurement = {

    var roundTrips = 0
    var bandwidth  = 0L

    // Messaging queues
    var toR1: Set[Event[T]] = replica2.hashDAG.getCurrentHeadsIDs.map(replica2.hashDAG.events)
    var toR2: Set[Event[T]] = replica1.hashDAG.getCurrentHeadsIDs.map(replica1.hashDAG.events)

    // Missing dependencies that need to be requested
    var needFromR1: Set[String] = Set.empty
    var needFromR2: Set[String] = Set.empty

    var receivedByR1: List[Event[T]] = Nil
    var receivedByR2: List[Event[T]] = Nil

    while toR1.nonEmpty || toR2.nonEmpty || needFromR1.nonEmpty || needFromR2.nonEmpty do {

      // Replica 1 receives
      receivedByR1 = receivedByR1 ++ toR1
      for ev <- toR1 do {
        if !replica1.hashDAG.contains(ev.id) then {
          for p <- ev.dependencies if !replica1.hashDAG.contains(p) && !receivedByR1.map(_.id).contains(p) do
              needFromR2 += p
        }
      }
      bandwidth += toR1.toList.map(writeToArray(_).length).sum
      toR1 = Set.empty

      toR2 ++= needFromR1.map(id => replica1.hashDAG.events(id))
      toR2 ++= toR2.flatMap(e => replica1.hashDAG.getNDependencies(e.id, dependencyPerRoundTrip)).map(id =>
        replica1.hashDAG.events(id)
      )
      bandwidth += needFromR1.toList.map(id => Base64.getDecoder.decode(id).length).sum
      needFromR1 = Set.empty

      //   Replica 2 receives
      receivedByR2 = receivedByR2 ++ toR2
      for ev <- toR2 do {
        if !replica2.hashDAG.contains(ev.id) then {
          for p <- ev.dependencies if !replica2.hashDAG.contains(p) && !receivedByR2.map(_.id).contains(p) do
              needFromR1 += p
        }
      }
      bandwidth += toR2.toList.map(writeToArray(_).length).sum
      toR2 = Set.empty

      toR1 ++= needFromR2.map(id => replica2.hashDAG.events(id))
      toR1 ++= toR1.flatMap(e => replica2.hashDAG.getNDependencies(e.id, dependencyPerRoundTrip)).map(id =>
        replica2.hashDAG.events(id)
      )
      bandwidth += needFromR2.toList.map(id => Base64.getDecoder.decode(id).length).sum
      needFromR2 = Set.empty

      roundTrips += 1
    }

    // Correctness checks
    val r1IDs = replica1.hashDAG.getIDs
    val r2IDs = replica2.hashDAG.getIDs
    assert(r2IDs.subsetOf(r1IDs ++ receivedByR1.map(_.id)))
    assert(r1IDs.subsetOf(r2IDs ++ receivedByR2.map(_.id)))

    // Final delta bandwidth
    val events1              = replica1.hashDAG.events.values.toSet
    val events2              = replica2.hashDAG.events.values.toSet
    val deltaBandwidth: Long = ((events1 -- events2) ++ (events2 -- events1)).toList.map(writeToArray(_).length).sum

    Measurement(
      method = "RSync",
      dagSize = size,
      diff = diff,
      roundTrips = roundTrips,
      bandwidth = bandwidth,
      delta = deltaBandwidth,
      codedSymbolPerRoundTrip = dependencyPerRoundTrip,
      deltaSize = deltaSize
    )
  }

  def rsyncV2[T, R <: Replica[T, R]](
      replica1: Replica[T, R],
      replica2: Replica[T, R],
      size: Int,
      diff: Float,
      dependencyPerRoundTrip: Int = 1,
      deltaSize: Int,
  )(using JsonValueCodec[Event[T]]): Measurement = {

    var roundTrips = 0
    var bandwidth  = 0L

    case class Request(id: String)
    case class Response(id: String, dependencies: Map[String, Set[String]])

    // Messaging queues
    var toR1: Set[Response] =
      replica2.hashDAG.getCurrentHeads.map(e => Response(e.id, Map.apply((e.id, e.dependencies))))
    var toR2: Set[Response] =
      replica1.hashDAG.getCurrentHeads.map(e => Response(e.id, Map.apply((e.id, e.dependencies))))

    // Missing dependencies that need to be requested
    var needFromR1: Set[Request] = Set.empty
    var needFromR2: Set[Request] = Set.empty

    var receivedByR1: Set[Response] = Set.empty
    var receivedByR2: Set[Response] = Set.empty
    var neededAcc1: Set[String]     = Set.empty
    var neededAcc2: Set[String]     = Set.empty

    while toR1.nonEmpty || toR2.nonEmpty || needFromR1.nonEmpty || needFromR2.nonEmpty do {

      // Replica 1 receives
      receivedByR1 = receivedByR1 ++ toR1
      for response <- toR1 do {
        if !replica1.hashDAG.contains(response.id) then {
          neededAcc1 += response.id
          neededAcc1 ++= response.dependencies.keySet
          val req = response.dependencies.values.flatten.toSet.filter(id =>
            !replica1.hashDAG.contains(id) && !neededAcc1.contains(id)
          )
          neededAcc1 ++= req
          needFromR2 = needFromR2 ++ req.map(r => Request(r))
        }

      }

      for r <- toR1 do {
        bandwidth += Base64.getDecoder.decode(r.id).length
        for (k, v) <- r.dependencies do
            bandwidth += Base64.getDecoder.decode(k).length
            bandwidth += v.toList.map(id => Base64.getDecoder.decode(id).length).sum
      }
      toR1 = Set.empty

      // toR2 = toR2 ++ needFromR1
      toR2 = toR2 ++ needFromR1.map(request =>
        Response(
          request.id,
          replica1.hashDAG.getNDependencies(request.id, dependencyPerRoundTrip).map(id =>
            replica1.hashDAG.events(id)
          ).map(e => e.id -> e.dependencies).toMap
        )
      )
      bandwidth += needFromR1.toList.map(request => Base64.getDecoder.decode(request.id).length).sum
      needFromR1 = Set.empty

      //   Replica 2 receives
      receivedByR2 = receivedByR2 ++ toR2
      for response <- toR2 do {
        if !replica2.hashDAG.contains(response.id) then {
          neededAcc2 += response.id
          neededAcc2 ++= response.dependencies.keySet
          val req = response.dependencies.values.flatten.toSet.filter(id =>
            !replica2.hashDAG.contains(id) && !neededAcc2.contains(id)
          )
          neededAcc2 ++= req
          needFromR1 = needFromR1 ++ req.map(r => Request(r))
        }

      }

      for r <- toR2 do {
        bandwidth += Base64.getDecoder.decode(r.id).length
        for (k, v) <- r.dependencies do
            bandwidth += Base64.getDecoder.decode(k).length
            bandwidth += v.toList.map(id => Base64.getDecoder.decode(id).length).sum
      }
      toR2 = Set.empty

      // toR1 = toR1 ++ needFromR2
      toR1 = toR1 ++ needFromR2.map(request =>
        Response(
          request.id,
          replica2.hashDAG.getNDependencies(request.id, dependencyPerRoundTrip).map(id =>
            replica2.hashDAG.events(id)
          ).map(e => e.id -> e.dependencies).toMap
        )
      )
      bandwidth += needFromR2.toList.map(request => Base64.getDecoder.decode(request.id).length).sum
      needFromR2 = Set.empty

      roundTrips += 1
    }

    bandwidth += neededAcc1.filter(id => !replica1.hashDAG.contains(id)).toList.map(e =>
      writeToArray(replica2.hashDAG.events(e)).length
    ).sum
    bandwidth += neededAcc2.filter(id => !replica2.hashDAG.contains(id)).toList.map(e =>
      writeToArray(replica1.hashDAG.events(e)).length
    ).sum

    // Correctness checks
    val r1IDs = replica1.hashDAG.getIDs
    val r2IDs = replica2.hashDAG.getIDs
    assert((r1IDs ++ neededAcc1) == (r2IDs ++ neededAcc2))

    // Final delta bandwidth
    val events1              = replica1.hashDAG.events.values.toSet
    val events2              = replica2.hashDAG.events.values.toSet
    val deltaBandwidth: Long = ((events1 -- events2) ++ (events2 -- events1)).toList.map(writeToArray(_).length).sum

    Measurement(
      method = "RSyncv2",
      dagSize = size,
      diff = diff,
      roundTrips = roundTrips,
      bandwidth = bandwidth,
      delta = deltaBandwidth,
      codedSymbolPerRoundTrip = dependencyPerRoundTrip,
      deltaSize = deltaSize
    )
  }

  def rsyncThreaded[T, R <: Replica[T, R]](replica1: R, replica2: R)(using JsonValueCodec[R]): (Int, Int) = {
    val stats = SyncStats()

    val t1 = new PingPongSync(replica1, "replica1", "replica2", stats)
    val t2 = new PingPongSync(replica2, "replica2", "replica1", stats)

    t1.start()
    t2.start()

    t1.join()
    t2.join()

    stats.results
  }

  def syncRIBLT[T, R <: Replica[T, R]](
      replica1: R,
      replica2: R,
      codedSymbolsPerRoundTrip: Int = 10,
      size: Int,
      diff: Float,
      deltaSize: Int
  )(using
      JsonValueCodec[Event[T]]
  ): Measurement = {
    val riblt1     = RIBLT[String]()
    val riblt2     = RIBLT[String]()
    var roundTrips = 0
    var bandwidth  = 0L

    for id <- replica1.hashDAG.getIDs do
        riblt1.addSymbol(id)

    for id <- replica2.hashDAG.getIDs do
        riblt2.addSymbol(id)

    while !riblt1.isDecoded do {
      val cs = riblt2.produceNextCodedSymbolsAsBytes(codedSymbolsPerRoundTrip)
      riblt1.addCodedSymbolsAsBytes(cs)
      roundTrips += 1
      bandwidth = cs.foldLeft(bandwidth)((b, arr) => b + arr.length)
      // println(roundTrips)
    }

    val localEvents     = riblt1.localSymbols.map(s => replica1.hashDAG.events(s.value))
    val remoteEventsIDs = riblt1.remoteSymbols.map(s => s.value)
    val remoteEvents    = riblt1.remoteSymbols.map(s => replica2.hashDAG.events(s.value))

    roundTrips += 1
    bandwidth += remoteEventsIDs.map(id => Base64.getDecoder.decode(id).length).sum
    val tmp = (localEvents ++ remoteEvents).foldLeft(0)((b, e) => b + writeToArray(e).length)
    bandwidth += tmp

    val replica1IDs = replica1.hashDAG.getIDs
    val replica2IDs = replica2.hashDAG.getIDs

    // println(s"SyncRIBLT: diff = ${(replica1IDs -- replica2IDs).size}, total = ${replica1IDs.size}, ${replica2IDs.size}")
    // println(s"roundTrips = $roundTrips, bandwidth = $bandwidth")
    // println(s"coded symbols per roundtrip = $codedSymbolsPerRoundTrip")

    val events1              = replica1.hashDAG.events.values.toSet
    val events2              = replica2.hashDAG.events.values.toSet
    val d                    = ((events1 -- events2) ++ (events2 -- events1)).toList
    val deltaBandwidth: Long = d.map(x => writeToArray(x).length).sum

    Measurement(
      method = "RIBLT",
      dagSize = size,
      diff = diff,
      roundTrips = roundTrips,
      bandwidth = bandwidth,
      delta = deltaBandwidth,
      codedSymbolPerRoundTrip = codedSymbolsPerRoundTrip,
      deltaSize = deltaSize
    )
  }

  def syncBloom[T, R <: Replica[T, R]](
      replica1: R,
      replica2: R,
      falsePositiveRate: Float,
      size: Int,
      diff: Float,
      deltaSize: Int
  )(using JsonValueCodec[Event[T]]): Measurement = {
    var roundTrips = 0
    var bandwidth  = 0L

    val IDs1              = replica1.hashDAG.events.keySet
    val expectedElements1 = IDs1.size
    val bf1               = BloomFilter[String](expectedElements1, falsePositiveRate)

    for id <- IDs1 do
        bf1.add(id)

    val IDs2              = replica2.hashDAG.events.keySet
    val expectedElements2 = IDs2.size
    val bf2               = BloomFilter[String](expectedElements2, falsePositiveRate)

    for id <- IDs2 do
        bf2.add(id)

    val out1 = ByteArrayOutputStream()
    bf1.writeTo(out1)
    bandwidth += out1.size()

    val out2 = ByteArrayOutputStream()
    bf2.writeTo(out2)
    bandwidth += out2.size()

    roundTrips += 1

    var toR2 = List.empty[Event[T]]
    for id <- IDs1 do
        if !bf2.mightContain(id) then {
          toR2 = toR2 :+ replica1.hashDAG.events(id)
          toR2 = toR2 :++ replica1.hashDAG.getAllSuccessors(id).map(i => replica1.hashDAG.events(i))
        }

    var toR1 = List.empty[Event[T]]
    for id <- IDs2 do
        if !bf1.mightContain(id) then
            toR1 = toR1 :+ replica2.hashDAG.events(id)
            toR1 = toR1 :++ replica2.hashDAG.getAllSuccessors(id).map(i => replica2.hashDAG.events(i))


    roundTrips += 1
    bandwidth += toR2.map(e => writeToArray(e).length).sum
    bandwidth += toR1.map(e => writeToArray(e).length).sum

    val s1     = (toR1.map(e => e.id) ++ IDs1).toSet
    val s2     = (toR2.map(e => e.id) ++ IDs2).toSet
    val synced = s1 == s2

    var receivedByR1: List[Event[T]] = toR1
    var receivedByR2: List[Event[T]] = toR2

    var bloomFilterFailed = -1

    if !synced then {
      println("bloom filter had false positives, falling back to ping-pong")
      bloomFilterFailed = 1
      // Missing dependencies that need to be requested
      var needFromR1: Set[String] = Set.empty
      for e <- toR2 do {
        for d <- e.dependencies do
            if !replica2.hashDAG.contains(d) && !receivedByR2.map(ev => ev.id).contains(d) then
                needFromR1 = needFromR1 + d
      }

      var needFromR2: Set[String] = Set.empty
      for e <- toR1 do {
        for d <- e.dependencies do
            if !replica1.hashDAG.contains(d) && !receivedByR1.map(ev => ev.id).contains(d) then
                needFromR2 = needFromR2 + d
      }

      toR1 = List.empty
      toR2 = List.empty

      while toR1.nonEmpty || toR2.nonEmpty || needFromR1.nonEmpty || needFromR2.nonEmpty do {

        // Replica 1 receives
        receivedByR1 = receivedByR1 ++ toR1
        for ev <- toR1 do {
          if !replica1.hashDAG.contains(ev.id) then {
            for p <- ev.dependencies if !replica1.hashDAG.contains(p) && !receivedByR1.map(_.id).contains(p) do
                needFromR2 += p
          }
        }
        bandwidth += toR1.map(writeToArray(_).length).sum
        toR1 = List.empty

        toR2 ++= needFromR1.map(id => replica1.hashDAG.events(id))
        bandwidth += needFromR1.toList.map(id => Base64.getDecoder.decode(id).length).sum
        needFromR1 = Set.empty

        //   Replica 2 receives
        receivedByR2 = receivedByR2 ++ toR2
        for ev <- toR2 do {
          if !replica2.hashDAG.contains(ev.id) then {
            for p <- ev.dependencies if !replica2.hashDAG.contains(p) && !receivedByR2.map(_.id).contains(p) do
                needFromR1 += p
          }
        }
        bandwidth += toR2.map(writeToArray(_).length).sum
        toR2 = List.empty

        toR1 ++= needFromR2.map(id => replica2.hashDAG.events(id))
        bandwidth += needFromR2.toList.map(id => Base64.getDecoder.decode(id).length).sum
        needFromR2 = Set.empty

        roundTrips += 1
      }

    }

    assert((receivedByR1.map(e => e.id) ++ IDs1).toSet == (receivedByR2.map(e => e.id) ++ IDs2).toSet)

    val events1              = replica1.hashDAG.events.values.toSet
    val events2              = replica2.hashDAG.events.values.toSet
    val d                    = ((events1 -- events2) ++ (events2 -- events1)).toList
    val deltaBandwidth: Long = d.map(x => writeToArray(x).length).sum

    Measurement(
      method = "BLOOM",
      dagSize = size,
      diff = diff,
      roundTrips = roundTrips,
      bandwidth = bandwidth,
      delta = deltaBandwidth,
      codedSymbolPerRoundTrip = -1,
      deltaSize = deltaSize,
      falsePositiveRate = falsePositiveRate,
      bloomFilterFailed = bloomFilterFailed
    )
  }

  @main def main(): Unit = {
    given c1: JsonValueCodec[Event[Op[String]]] = JsonCodecMaker.make

    given c2: JsonValueCodec[Event[Int]] = JsonCodecMaker.make

    var r1                       = ORSet[String]()
    var r2                       = ORSet[String]()
    val size                     = 1000
    val diff                     = 0.1f
    val deltaSize                = 10
    val dependencyPerRoundTrip   = 1
    val codedSymbolsPerRoundTrip = 1
    val gen                      = ReplicaGenerator.generate(size, diff, r1, r2, deltaSize)

    // val t = r1.add("A")
    // r1.merge(t)

    r1 = gen._1
    r2 = gen._2

    //r1 = ReplicaExamples.Example1.replica1
    //r2 = ReplicaExamples.Example1.replica2

    /*println(syncPingPong(r1, r2, size, diff, dependencyPerRoundTrip, deltaSize))
    println(syncPingPongv2(r1, r2, size, diff, dependencyPerRoundTrip, deltaSize))

    println(syncPingPong(r1, r2, size, diff, dependencyPerRoundTrip + 18, deltaSize))
    println(syncPingPongv2(r1, r2, size, diff, dependencyPerRoundTrip + 18, deltaSize))

    println(syncPingPong(r1, r2, size, diff, dependencyPerRoundTrip + 37, deltaSize))
    println(syncPingPongv2(r1, r2, size, diff, dependencyPerRoundTrip + 37, deltaSize))*/

    // println(syncBloom(r1, r2, 10f, size, diff, deltaSize))
    // println(syncBloom(r1, r2, 0.00001f, size, diff, deltaSize))
    println(syncBloom(r1, r2, 0.1f, size, diff, deltaSize))
    println(rsync(r1, r2, size, diff, dependencyPerRoundTrip, deltaSize))
    println(rsyncV2(r1, r2, size, diff, dependencyPerRoundTrip, deltaSize))
    println(syncRIBLT(r1, r2, 10, size, diff, deltaSize))


    /*println(syncBloom(r1, r2, 0.01f, size, diff, deltaSize))
    println(syncBloom(r1, r2, 0.001f, size, diff, deltaSize))
    println(syncBloom(r1, r2, 0.0001f, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 100, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 500, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 1000, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 2000, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 3000, size, diff, deltaSize))
    println(syncRIBLT(r1, r2, 4000, size, diff, deltaSize))*/

    // println(syncPingPong(r1, r2, size, diff, 1000, 1000))
    // println(syncPingPong(r1, r2, 10, 1, 10))
    // println(syncPingPong(r1, r2, 10, 1, 20))
    // println(syncPingPong(r1, r2, 10, 1, 100))

    // println(syncRIBLT(r1, r2, 20, 100, 1, 1))

    // println(syncRIBLT(r1, r2))

    /*val replica1 = ReplicaExamples.Example1.replica1
      val replica2 = ReplicaExamples.Example1.replica2

      // println(syncNaively(replica1, replica2))
      // println(syncPingPong(replica1, replica2))
      // println(syncRIBLT(replica1, replica2))
      // val res = syncPingPongThreaded(replica1, replica2)
      // println(s"(${res._1 / 2}, ${res._2})")

      println("------")
      val replica3 = ReplicaExamples.Example2.replica1
      val replica4 = ReplicaExamples.Example2.replica2
      syncPingPong(replica3, replica4)
      syncRIBLT(replica3, replica4)
      // val res1 = syncPingPongThreaded(replica3, replica4)
      // println(s"(${res1._1 / 2}, ${res1._2})")

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
      // val res2 = syncPingPongThreaded(replica5, replica6)
      // println(s"(${res2._1 / 2}, ${res2._2})")*/
  }
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
