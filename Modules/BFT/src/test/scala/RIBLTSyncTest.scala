import riblt.SessionType.{receiver, sender}
import datatypes.{ORSet, Op}
import munit.FunSuite
import riblt.RIBLTSync
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import crypto.Ed25519Util

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

class RIBLTSyncTest extends munit.FunSuite:
  override def munitTimeout: Duration = 5.minutes

  test("basic") {
    var crdt1 = ORSet[String]()
    crdt1 = crdt1.merge(crdt1.add("hello"))
    crdt1 = crdt1.merge(crdt1.add("hola"))
    crdt1 = crdt1.merge(crdt1.add("Gday Mate"))

    var crdt2 = ORSet[String]()
    crdt2 = crdt2.merge(crdt2.add("hi"))
    crdt2 = crdt2.merge(crdt2.add("bonjour"))
    crdt2 = crdt2.merge(crdt2.add("hallo"))

    var crdt3 = ORSet[String]()
    crdt3 = crdt3.merge(crdt3.add("Guten Tag"))
    crdt3 = crdt3.merge(crdt3.add("Ni hao"))
    crdt3 = crdt3.merge(crdt3.add("Konichiwa"))


    val sync1 = RIBLTSync(crdt1, Map.empty, "replica_1")
    val sync2 = RIBLTSync(crdt2, Map.empty, "replica_2")
    val sync3 = RIBLTSync(crdt3, Map.empty, "replica_3")

    val t1 = sync1.startSession(sync2.replicaID, sessionType=sender)
    val t2 = sync2.startSession(sync1.replicaID, sessionType=receiver)

    val t3 = sync3.startSession(sync2.replicaID, sessionType=receiver)
    val t4 = sync2.startSession(sync3.replicaID, sessionType=sender)

    t2.start()
    t1.start()
    t3.start()
    t4.start()

    t1.join()
    t2.join()
    t3.join()
    t4.join()

    crdt1 = sync1.replica
    crdt2 = sync2.replica
    crdt3 = sync3.replica

    //sync2.startSession(sync3.id, sessionType=sender)
    //sync3.startSession(sync2.id, sessionType=receiver)

    println(crdt1.elements.keySet)
    println(crdt2.elements.keySet)
    println(crdt3.elements.keySet)

  }
