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
    case Register(uid: Uid)
    case Announce(topic: String, descriptors: Set[ChannelConnectInfo])
    case LookupPeer(requestId: Uid, uid: Uid)
    case PeerInfo(requestId: Uid, uid: Uid, topics: Map[String, Set[ChannelConnectInfo]])
    case LookupTopic(requestId: Uid, topic: String, count: Int)
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

  private def describe(conn: Connection): String =
    s"${conn.getClass.getSimpleName}@${System.identityHashCode(conn)} ${conn.info}"

  private def sendOrDisconnect(conn: Connection, msg: Message)(onFailure: => Unit = disconnect(conn)): Unit =
    conn.send(ArrayMessageBuffer(writeToArray(msg))).run {
      case Success(_)  => ()
      case Failure(ex) =>
        log(s"send failed ex=${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}")
        onFailure
    }

  def stop(): Unit = abort.abort()

  def addIncomingConnection(latent: LatentConnection): Unit =
    latent.prepare(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def peerTopics(uid: Uid): Map[String, Set[channels.ChannelConnectInfo]] =
    announcementsByUid.getOrElse(uid, Map.empty)

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
    log(s"register ${Uid.unwrap(uid)} via ${describe(conn)}")
  }

  private val receive: Receive =
    (conn: Connection) => {
      case Success(buffer) =>
        val message = readFromArray[Message](buffer.asArray)
        log(s"received $message via ${describe(conn)}")
        message match
            case Message.Register(uid) =>
              register(uid, conn)
              sendOrDisconnect(conn, Message.Register(uid))()

            case Message.Announce(topic, descriptors) =>
              uidByConn.get(conn) match
                  case Some(uid) =>
                    announcementsByUid.update(uid, announcementsByUid.getOrElse(uid, Map.empty).updated(topic, descriptors))
                    log(s"announce uid=${Uid.unwrap(uid)} topic=$topic descriptors=${descriptors.size}")
                  case None =>
                    log(s"announce ignored for unknown connection topic=$topic descriptors=$descriptors via ${describe(conn)}")

            case Message.LookupPeer(requestId, uid) =>
              sendOrDisconnect(conn, Message.PeerInfo(requestId, uid, peerTopics(uid)))()

            case Message.LookupTopic(requestId, topic, count) =>
              sendOrDisconnect(conn, Message.TopicInfo(requestId, topic, randomTopicPeers(topic, count)))()

            case msg @ Message.Offer(from, to, _) if uidByConn.get(conn).contains(from) =>
              clientsByUid.get(to).foreach(target => sendOrDisconnect(target, msg)())
              log(s"offer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

            case msg @ Message.Answer(from, to, _) if uidByConn.get(conn).contains(from) =>
              clientsByUid.get(to).foreach(target => sendOrDisconnect(target, msg)())
              log(s"answer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

            case _ => ()

      case Failure(_) =>
        disconnect(conn)
    }
}
