import riblt.SessionType.{receiver, sender}
import datatypes.{ORSet, Op}
import munit.FunSuite
import riblt.RIBLTSyncWithThreads
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import crypto.Ed25519Util
import network.Network

import java.security.{PrivateKey, PublicKey}
import scala.concurrent.duration.{Duration, DurationInt}

given JsonValueCodec[ORSet[String]] = JsonCodecMaker.make

given JsonValueCodec[PublicKey] = new JsonValueCodec[PublicKey] {
  override def encodeValue(key: PublicKey, out: JsonWriter): Unit =
    out.writeBase64Val(Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(key).getBytes, false)
  override def decodeValue(in: JsonReader, default: PublicKey): PublicKey =
    Ed25519Util.base64PublicKeyBytesToPublicKey(String(in.readBase64AsBytes(Array.empty[Byte])))

  override def nullValue: PublicKey = null
}

given JsonValueCodec[PrivateKey] = new JsonValueCodec[PrivateKey] {
  override def encodeValue(key: PrivateKey, out: JsonWriter): Unit =
    out.writeRawVal(Ed25519Util.privateKeyToRawPrivateKeyBytes(key))
  override def decodeValue(in: JsonReader, default: PrivateKey): PrivateKey =
    Ed25519Util.rawPrivateKeyBytesToPrivateKey(in.readRawValAsBytes())

  override def nullValue: PrivateKey = null
}

class RIBLTSyncWithThreadsTest extends munit.FunSuite:
   override def munitTimeout: Duration = 5.minutes

   test("basic") {
     /* var crdt0 = ORSet[String]()
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

     val sync0 = RIBLTSyncWithThreads(crdt0, Map.empty, "replica_0")
     val sync1 = RIBLTSyncWithThreads(crdt1, Map.empty, "replica_1")
     val sync2 = RIBLTSyncWithThreads(crdt2, Map.empty, "replica_2")
     val sync3 = RIBLTSyncWithThreads(crdt3, Map.empty, "replica_3")

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
     ) */
   }
