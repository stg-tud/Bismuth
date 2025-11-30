package benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import datatypes.{ORSet, Replica}
import network.Network
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}

import java.security.{PrivateKey, PublicKey}
import scala.util.Random

given JsonValueCodec[ORSet[String]] = JsonCodecMaker.make
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

  def syncPingPong[T, R <: Replica[T, R]](replica1: Replica[T, R], replica2: Replica[T, R]): (Int, Int) = {
    var roundTrips = 0
    var bandwidth  = 0

    // Step 1: Exchange Heads of the HashDAG
    val heads1 = replica1.hashDAG.getCurrentHeadsIDs
    val heads2 = replica2.hashDAG.getCurrentHeadsIDs
    roundTrips += 1
    bandwidth += (heads1.size + heads2.size)

    // Step 2: If the heads are different, syncdef syncWithB*/
    if heads1 != heads2 then
        val exclusiveHeads1 = heads1 -- heads2
        val exclusiveHeads2 = heads2 -- heads1
        val mutualHeads     = heads1.intersect(heads2)
        val IDs1            = replica1.hashDAG.getIDs
        val IDs2            = replica2.hashDAG.getIDs

        var replica1IDsAfterSync = IDs1
        var replica2IDsAfterSync = IDs2
        var missing1             = exclusiveHeads2
        var missing2             = exclusiveHeads1
        var tmp1                 = 0
        var tmp2                 = 0

        while missing2.nonEmpty do
            var tmp = Set.empty[String]
            for head <- missing2 do {
              if !IDs2.contains(head) then {
                replica2IDsAfterSync = replica2IDsAfterSync + head
                tmp = tmp ++ replica1.hashDAG.getDirectDependencies(head)
              }
            }

            missing2 = tmp
            tmp1 += 1

        while missing1.nonEmpty do
            var tmp = Set.empty[String]
            for head <- missing1 do {
              if !IDs1.contains(head) then {
                replica1IDsAfterSync = replica1IDsAfterSync + head
                tmp = tmp ++ replica2.hashDAG.getDirectDependencies(head)
              }
            }

            missing1 = tmp
            tmp2 += 1

        roundTrips += roundTrips + List(tmp1, tmp2).max
        bandwidth += (replica1IDsAfterSync -- IDs1).size + (replica2IDsAfterSync -- IDs2).size

        assert(replica1IDsAfterSync == replica1IDsAfterSync)

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

  def syncRIBLT[T, R <: Replica[T, R]](replica1: R, replica2: R): (Int, Int) = {
    val riblt1     = RIBLT[String]()
    val riblt2     = RIBLT[String]()
    var roundTrips = 0

    for id <- replica1.hashDAG.getIDs do
        riblt1.addSymbol(id)

    for id <- replica2.hashDAG.getIDs do
        riblt2.addSymbol(id)

    while !riblt1.isDecoded do {
      val cs = riblt2.produceNextCodedSymbol
      riblt1.addCodedSymbol(cs)
      roundTrips += 1
      // println(r)
    }

    (roundTrips, riblt1.localSymbols.size + riblt1.remoteSymbols.size + roundTrips)
  }

  @main def main(): Unit =
      var replica1 = ORSet[String]()
      var replica2 = ORSet[String]()
      var replica3 = ORSet[String]()
      var replica4 = ORSet[String]()

      // A
      replica1 = replica1.merge(replica1.add("Hello"))
      // B
      replica1 = replica1.merge(replica1.add("Hi"))

      replica2 = replica2.merge(replica1)
      replica3 = replica3.merge(replica1)
      replica4 = replica4.merge(replica1)

      // C
      replica1 = replica1.merge(replica1.add("Guten Tag"))

      // D
      replica1 = replica1.merge(replica1.add("Guten Tag"))

      // J
      replica3 = replica3.merge(replica3.add("Moin"))

      // K
      replica3 = replica3.merge(replica3.add("Moin"))

      // F
      replica2 = replica2.merge(replica2.add("qqq"))

      // G
      replica2 = replica2.merge(replica2.add("ttt"))

      replica1 = replica1.merge(replica3)
      replica2 = replica2.merge(replica3)

      // E
      replica1 = replica1.merge(replica1.add("aaa"))

      // L
      replica3 = replica3.merge(replica3.add("Moin"))

      // M
      replica3 = replica3.merge(replica3.add("Moin"))

      replica1 = replica1.merge(replica3)

      println(syncNaively(replica1, replica2))
      println(syncPingPong(replica1, replica2))
      println(syncRIBLT(replica1, replica2))
      println(syncPingPongThreaded(replica1, replica2))

}
