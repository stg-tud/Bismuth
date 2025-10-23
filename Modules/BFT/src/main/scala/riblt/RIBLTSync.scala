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
import network.Tag.{CODED_SYMBOLS, DELTA, REQUEST_DELTA, SEND_CODED_SYMBOLS, SYNC_DONE}
import riblt.RIBLTSync.{codec1, codec2}

class RIBLTSync[T, R <: Replica[T, R]](
                                        var replica: R,
                                        var RIBLTSessions: Map[Array[Byte], Session] = Map.empty,
                                        val replicaID: Array[Byte]
                                      ):

  def startSession(id: Array[Byte], sessionType: SessionType)(using JsonValueCodec[R]): Thread = {
    val riblt = RIBLT[String]()
    for id <- replica.hashDAG.getIDs do
      riblt.addSymbol(id)

    this.RIBLTSessions = this.RIBLTSessions.updated(id, Session(sessionType, riblt, false, false))

    val thread = new Thread(() => {
      var keepSession = true
      if sessionType == sender then {
        val msg = Message(
          CODED_SYMBOLS,
          this.replicaID,
          writeToArray(RIBLTSessions(id).riblt.produceNextCodedSymbolsAsBytes())
        )
        println(s"putting first message to ${id.mkString("Array(", ", ", ")")}")
        Network.put(id, writeToArray(msg))
      }

      while keepSession do
        println(s"reading from ${this.replicaID.mkString("Array(", ", ", ")")}")
        val messages = Network.get(this.replicaID)
        if messages.nonEmpty then {
          println("found a message!!")
          println(s"message ${messages.toString()}")
        }
        keepSession = handleMessages(messages)

    })

    thread
  }

  def handleMessages(queue: Queue[Array[Byte]])(using JsonValueCodec[R]): Boolean = {
    var keepSession = true

    val name = Thread.currentThread().getName

    for bytes <- queue do {
      val message = readFromArray[Message](bytes)

      message.tag match {
        case SEND_CODED_SYMBOLS =>
          println(s"thread $name requested coded symbols")
          val msg = Message(
            CODED_SYMBOLS,
            this.replicaID,
            writeToArray(RIBLTSessions(message.sender).riblt.produceNextCodedSymbolsAsBytes())
          )
          Network.put(message.sender, writeToArray(msg))
        case CODED_SYMBOLS =>
          println(s"thread $name is receiving coded symbols")
          val codedSymbols = readFromArray[List[Array[Byte]]](message.payload)
          val riblt        = this.RIBLTSessions(message.sender).riblt
          riblt.addCodedSymbolsAsBytes(codedSymbols)
          if riblt.isDecoded then
            RIBLTSessions(message.sender).isDecoded = true

        case DELTA =>
          println(s"thread $name is sending deltas")
          val delta = readFromArray[R](message.payload)
          this.replica = this.replica.merge(delta)
        case REQUEST_DELTA =>
          println(s"thread $name requested to send delta")
          val ids   = readFromArray[List[String]](message.payload)
          val delta = replica.generateDelta(ids)
          val msg   = Message(
            DELTA,
            this.replicaID,
            writeToArray(delta)
          )
          Network.put(message.sender, writeToArray(msg))
        case SYNC_DONE =>
          println(s"thread $name is ending sync")
          keepSession = false
      }
    }

    keepSession
  }

object RIBLTSync:
  given codec1: JsonValueCodec[List[Array[Byte]]] = JsonCodecMaker.make
  given codec2: JsonValueCodec[List[String]]      = JsonCodecMaker.make

enum SessionType:
  case sender, receiver

class Session(
               var sessionType: SessionType,
               var riblt: RIBLT[String],
               var isDecoded: Boolean,
               var isSynced: Boolean
             )
