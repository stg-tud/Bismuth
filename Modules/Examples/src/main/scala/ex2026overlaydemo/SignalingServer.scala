package ex2026overlaydemo

import channels.JsoniterCodecs.given
import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, Connector, Listener, Receive}
import SignalingServer.Message
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid

import scala.collection.mutable
import scala.util.{Failure, Success}

object SignalingServer {
  case class Session(descType: String, sdp: String)

  enum Message {
    case Register(uid: Uid)
    case Offer(from: Uid, to: Uid, session: Session)
    case Answer(from: Uid, to: Uid, session: Session)
  }

  given codecSignalMessage: JsonValueCodec[Message] = JsonCodecMaker.make
}

class SignalingServer(debug: Boolean) {
  private val abort        = Abort()
  private val clientsByUid = mutable.Map.empty[Uid, Connection]
  private val uidByConn    = mutable.Map.empty[Connection, Uid]

  private def log(msg: => String): Unit = if debug then println(s"[signaling] $msg")

  private def disconnect(conn: Connection): Unit = {
    uidByConn.remove(conn).foreach { uid =>
      clientsByUid.remove(uid)
      log(s"disconnect ${Uid.unwrap(uid)}")
    }
    conn.close()
  }

  private def sendOrDisconnect(conn: Connection, msg: Message): Unit =
    conn.send(ByteBufferMessageBuffer(writeToArray(msg))).run {
      case Success(_) => ()
      case Failure(_) => disconnect(conn)
    }

  private def register(uid: Uid, conn: Connection): Unit = {
    uidByConn.get(conn).filter(_ != uid).foreach(clientsByUid.remove)
    clientsByUid.get(uid).filterNot(_ == conn).foreach(disconnect)
    uidByConn.update(conn, uid)
    clientsByUid.update(uid, conn)
  }

  private def relay(conn: Connection, from: Uid, to: Uid, msg: Message): Unit =
    if uidByConn.get(conn).contains(from) || !uidByConn.contains(conn) then {
      register(from, conn)
      clientsByUid.get(to).foreach(target => sendOrDisconnect(target, msg))
    }

  def stop(): Unit = abort.abort()

  def addIncomingConnection(connector: Connector): Unit =
    connector.connect(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def addIncomingConnection(listener: Listener): Unit =
    listener.listen(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  private val receive: Receive =
    (conn: Connection) => {
      case Success(buffer) =>
        readFromByteBuffer[Message](buffer.asByteBuffer) match
            case Message.Register(uid)             => register(uid, conn)
            case msg @ Message.Offer(from, to, _)  => relay(conn, from, to, msg)
            case msg @ Message.Answer(from, to, _) => relay(conn, from, to, msg)
      case Failure(_) =>
        disconnect(conn)
    }
}
