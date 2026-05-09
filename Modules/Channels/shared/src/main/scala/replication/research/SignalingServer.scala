package replication.research

import channels.{Abort, ArrayMessageBuffer, ChannelConnectInfo, Connection, LatentConnection, Receive}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.research.SignalingServer.Message

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object SignalingServer {
  case class Session(descType: String, sdp: String)

  enum Message {
    case Announce(uid: Uid, requestId: Uid, topic: String, descriptors: Set[ChannelConnectInfo], count: Int)
    case TopicInfo(requestId: Uid, topic: String, peers: Map[Uid, Set[ChannelConnectInfo]])
    case Offer(from: Uid, to: Uid, session: Session)
    case Answer(from: Uid, to: Uid, session: Session)
  }
}

class SignalingServer(
    debug: Boolean,
    random: Random = Random(0),
) {
  private val abort = Abort()

  private val clientsByUid       = mutable.Map.empty[Uid, Connection]
  private val uidByConn          = mutable.Map.empty[Connection, Uid]
  private val announcementsByUid = mutable.Map.empty[Uid, Map[String, Set[channels.ChannelConnectInfo]]]

  private def log(msg: => String): Unit = if debug then println(s"[signaling] $msg")

  private def sendOrDisconnect(conn: Connection, msg: Message)(onFailure: => Unit = disconnect(conn)): Unit =
    conn.send(ArrayMessageBuffer(writeToArray(msg))).run {
      case Success(_)  => ()
      case Failure(_)  => onFailure
    }

  def stop(): Unit = abort.abort()

  def addIncomingConnection(latent: LatentConnection): Unit =
    latent.prepare(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def topicPeers(topic: String): Map[Uid, Set[channels.ChannelConnectInfo]] =
    announcementsByUid.iterator.collect {
      case (uid, topics) if topics.contains(topic) => uid -> topics(topic)
    }.toMap

  def randomTopicPeers(topic: String, count: Int): Map[Uid, Set[channels.ChannelConnectInfo]] =
    random.shuffle(topicPeers(topic).toVector).take(math.max(0, count)).toMap

  private def disconnect(conn: Connection): Unit = {
    uidByConn.remove(conn).foreach { uid =>
      clientsByUid.remove(uid)
      announcementsByUid.remove(uid)
      log(s"disconnect ${Uid.unwrap(uid)}")
    }
    conn.close()
  }

  private def register(uid: Uid, conn: Connection): Unit = {
    uidByConn.get(conn).filter(_ != uid).foreach { previous =>
      clientsByUid.remove(previous)
      announcementsByUid.remove(previous)
    }
    clientsByUid.get(uid).filterNot(_ == conn).foreach(disconnect)
    uidByConn.update(conn, uid)
    clientsByUid.update(uid, conn)
  }

  private val receive: Receive =
    (conn: Connection) => {
      case Success(buffer) =>
        readFromArray[Message](buffer.asArray) match
            case Message.Announce(uid, requestId, topic, descriptors, count) =>
              register(uid, conn)
              announcementsByUid.update(uid, announcementsByUid.getOrElse(uid, Map.empty).updated(topic, descriptors))
              sendOrDisconnect(conn, Message.TopicInfo(requestId, topic, randomTopicPeers(topic, count)))()
            case msg @ Message.Offer(from, to, _) if uidByConn.get(conn).contains(from) =>
              clientsByUid.get(to).foreach(target => sendOrDisconnect(target, msg)())
            case msg @ Message.Answer(from, to, _) if uidByConn.get(conn).contains(from) =>
              clientsByUid.get(to).foreach(target => sendOrDisconnect(target, msg)())
            case _ => ()

      case Failure(_) =>
        disconnect(conn)
    }
}
