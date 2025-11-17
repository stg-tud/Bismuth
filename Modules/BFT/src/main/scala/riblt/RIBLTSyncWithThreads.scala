package riblt

import datatypes.{ORSet, Replica}
import network.{Message, Network}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_JsonValueCodec_CodedSymbol, given_Xorable_String}
import riblt.SessionType.{receiver, sender}
import network.Message.given_JsonValueCodec_Message
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import crypto.Ed25519Util
import network.Tag.{CODED_SYMBOLS, CODED_SYMBOLS_REQUEST, DELTA, REQUEST_DELTA}
import riblt.RIBLTSyncWithThreads.{codec1, codec2}

import java.security.{PrivateKey, PublicKey}

class RIBLTSyncWithThreads[T, R <: Replica[T, R]](
    var replica: R,
    val replicaID: String,
    var RIBLTSessions: Map[String, Session] = Map.empty,
    var syncThread: Option[Thread] = None
):

    def startSession(id: String, sessionType: SessionType)(using JsonValueCodec[R]): Thread = {
      val riblt = RIBLT[String]()
      for id <- replica.hashDAG.getIDs do
          riblt.addSymbol(id)

      this.RIBLTSessions = this.RIBLTSessions.updated(id, Session(sessionType, riblt, false, false))

      if sessionType == sender then {
        val msg = Message(
          CODED_SYMBOLS,
          this.replicaID,
          writeToArray(RIBLTSessions(id).riblt.produceNextCodedSymbolsAsBytes())
        )
        Network.put(id, writeToArray(msg))
      }

      if this.syncThread.isEmpty then {
        this.syncThread = Some(new Thread(() => {
          while this.RIBLTSessions.values.exists(s => !s.isSynced) do
              // println(s"reading from ${this.replicaID.mkString("Array(", ", ", ")")}")
              val message = Network.get(this.replicaID)
              if message.nonEmpty then {
                println(s"$replicaID:${Thread.currentThread().getName}:     has a new message")
              }
              handleMessage(message)

          println(s"$replicaID:${Thread.currentThread().getName}:    sync done, good bye!")
        }))
        this.syncThread.get.start()
      }

      this.syncThread.get
    }

    private def handleMessage(bytes: Array[Byte])(using JsonValueCodec[R]): Unit = {
      val name = Thread.currentThread().getName

      val message = readFromArray[Message](bytes)

      val sender: String = message.sender

      message.tag match {
        case CODED_SYMBOLS_REQUEST =>
          println(s"${this.replicaID}:${Thread.currentThread().getName}:    $sender requested coded symbols")
          val msg = Message(
            CODED_SYMBOLS,
            this.replicaID,
            writeToArray(RIBLTSessions(message.sender).riblt.produceNextCodedSymbolsAsBytes())
          )
          Network.put(message.sender, writeToArray(msg))
        case CODED_SYMBOLS =>
          println(s"${this.replicaID}:${Thread.currentThread().getName}:    $sender sent coded symbols")
          val codedSymbols = readFromArray[List[Array[Byte]]](message.payload)
          val riblt        = this.RIBLTSessions(message.sender).riblt
          riblt.addCodedSymbolsAsBytes(codedSymbols)
          if riblt.isDecoded then {
            println(s"${this.replicaID}:${Thread.currentThread().getName}:    is sending delta")
            RIBLTSessions(message.sender).isDecoded = true
            val msg1 = Message(
              DELTA,
              this.replicaID,
              writeToArray(replica.generateDelta(riblt.localSymbols.map(s => s.value)))
            )
            Network.put(message.sender, writeToArray(msg1))

            println(s"${this.replicaID}:${Thread.currentThread().getName}:    is sending a delta request to $sender")
            val msg2 = Message(
              REQUEST_DELTA,
              this.replicaID,
              writeToArray(riblt.remoteSymbols.map(s => s.value))
            )
            Network.put(message.sender, writeToArray(msg2))
          } else {
            println(s"${this.replicaID}:${Thread.currentThread().getName}:    is requesting coded symbols from $sender")
            val msg = Message(
              CODED_SYMBOLS_REQUEST,
              this.replicaID,
              Array.empty
            )
            Network.put(message.sender, writeToArray(msg))
          }
        case DELTA =>
          println(s"${this.replicaID}:${Thread.currentThread().getName}:    delta is received from $sender")
          val delta = readFromArray[R](message.payload)
          this.replica = this.replica.merge(delta)
          this.RIBLTSessions(message.sender).deltaReceived = true
        case REQUEST_DELTA =>
          println(s"${this.replicaID}:${Thread.currentThread().getName}:    a delta_request is received from $sender")
          val ids   = readFromArray[List[String]](message.payload)
          val delta = replica.generateDelta(ids)
          println(s"${this.replicaID}:${Thread.currentThread().getName}:    sending delta to $sender")
          val msg = Message(
            DELTA,
            this.replicaID,
            writeToArray(delta)
          )
          Network.put(message.sender, writeToArray(msg))
          this.RIBLTSessions(message.sender).deltaRequestReceived = true
      }

      for session <- this.RIBLTSessions.values do
          if session.sessionType == SessionType.sender && session.deltaReceived && session.deltaRequestReceived then
              session.isSynced = true
          if session.sessionType == receiver && session.deltaReceived then
              session.isSynced = true
    }

object RIBLTSyncWithThreads:
    given codec1: JsonValueCodec[List[Array[Byte]]] = JsonCodecMaker.make
    given codec2: JsonValueCodec[List[String]]      = JsonCodecMaker.make

    given JsonValueCodec[ORSet[String]] = JsonCodecMaker.make
    
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

enum SessionType:
    case sender, receiver

class Session(
    var sessionType: SessionType,
    var riblt: RIBLT[String] = RIBLT(),
    var isDecoded: Boolean = false,
    var isSynced: Boolean = false,
    var deltaReceived: Boolean = false,
    var deltaRequestReceived: Boolean = false
)
