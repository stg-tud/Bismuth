package riblt

import datatypes.Replica
import network.{Message, Network}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_JsonValueCodec_CodedSymbol, given_Xorable_String}
import riblt.SessionType.{receiver, sender}
import network.Message.given_JsonValueCodec_Message
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import java.security.PublicKey
import scala.collection.immutable.Queue
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import network.Tag.{CODED_SYMBOLS, DELTA, REQUEST_DELTA, CODED_SYMBOLS_REQUEST, SYNC_DONE}
import riblt.RIBLTSync.{codec1, codec2}

class RIBLTSync[T, R <: Replica[T, R]](
                                        var replica: R,
                                        var RIBLTSessions: Map[String, Session] = Map.empty,
                                        val replicaID: String
                                      ):

  def startSession(id: String, sessionType: SessionType)(using JsonValueCodec[R]): Thread = {
    val riblt = RIBLT[String]()
    for id <- replica.hashDAG.getIDs do
      riblt.addSymbol(id)

    this.RIBLTSessions = this.RIBLTSessions.updated(id, Session(sessionType, riblt, false, false))

    val thread = new Thread(() => {
      if sessionType == sender then {
        val msg = Message(
          CODED_SYMBOLS,
          this.replicaID,
          writeToArray(RIBLTSessions(id).riblt.produceNextCodedSymbolsAsBytes())
        )
        Network.put(id, writeToArray(msg))
      }

      while !this.RIBLTSessions(id).isSynced do
        //println(s"reading from ${this.replicaID.mkString("Array(", ", ", ")")}")
        val messages = Network.get(this.replicaID)
        if messages.nonEmpty then {
          println(s"$replicaID:    has a new message")
        }
        handleMessages(messages)
        Thread.sleep(1)
    })

    thread
  }

  def handleMessages(bytes: Array[Byte])(using JsonValueCodec[R]): Unit = {
    val name = Thread.currentThread().getName
    
    val message = readFromArray[Message](bytes)

    val sender: String = message.sender.toString

    message.tag match {
      case CODED_SYMBOLS_REQUEST =>
        println(s"${this.replicaID}:    ${sender} requested coded symbols")
        val msg = Message(
          CODED_SYMBOLS,
          this.replicaID,
          writeToArray(RIBLTSessions(message.sender).riblt.produceNextCodedSymbolsAsBytes())
        )
        Network.put(message.sender, writeToArray(msg))
      case CODED_SYMBOLS =>
        println(s"${this.replicaID}:    ${sender} sent coded symbols")
        val codedSymbols = readFromArray[List[Array[Byte]]](message.payload)
        val riblt        = this.RIBLTSessions(message.sender).riblt
        riblt.addCodedSymbolsAsBytes(codedSymbols)
        if riblt.isDecoded then {
          println(s"${this.replicaID}:    is sending delta")
          RIBLTSessions(message.sender).isDecoded = true
          val msg1 = Message(
            DELTA,
            this.replicaID,
            writeToArray(replica.generateDelta(riblt.localSymbols.map(s => s.value)))
          )
          Network.put(message.sender, writeToArray(msg1))

          println(s"${this.replicaID}:    is sending a delta request to ${sender}")
          val msg2 = Message(
            REQUEST_DELTA,
            this.replicaID,
            writeToArray(riblt.remoteSymbols.map(s => s.value))
          )
          Network.put(message.sender, writeToArray(msg2))
        } else {
          println(s"${this.replicaID}:    is requesting coded symbols from ${sender}")
          val msg = Message(
            CODED_SYMBOLS_REQUEST,
            this.replicaID,
            Array.empty
          )
          Network.put(message.sender, writeToArray(msg))
          println(s"thread $name is requesting more coded symbols")
        }
      case DELTA =>
        println(s"${this.replicaID}:    delta is received from ${sender}")
        val delta = readFromArray[R](message.payload)
        this.replica = this.replica.merge(delta)
        this.RIBLTSessions(message.sender).deltaReceived = true
      case REQUEST_DELTA =>
        println(s"${this.replicaID}:    a delta_request is received from ${sender}")
        val ids   = readFromArray[List[String]](message.payload)
        val delta = replica.generateDelta(ids)
        println(s"${this.replicaID}:    sending delta to ${sender}")
        val msg   = Message(
          DELTA,
          this.replicaID,
          writeToArray(delta)
        )
        Network.put(message.sender, writeToArray(msg))
        this.RIBLTSessions(message.sender).deltaRequestReceived = true
      case SYNC_DONE =>
        println(s"thread $name is ending sync")
    }

    for session <- this.RIBLTSessions.values do
      if session.sessionType == SessionType.sender && session.deltaReceived && session.deltaRequestReceived then
        session.isSynced = true
      if session.sessionType == receiver && session.deltaReceived then
        session.isSynced = true
  }

object RIBLTSync:
  given codec1: JsonValueCodec[List[Array[Byte]]] = JsonCodecMaker.make
  given codec2: JsonValueCodec[List[String]]      = JsonCodecMaker.make

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
