package replication.research

import channels.{Abort, ChannelConnectDescriptor, Connection, LatentConnection, Receive}
import rdts.base.Uid
import replication.research.SignalingServer.Message

import scala.collection.mutable
import scala.util.{Failure, Success}

object SignalingServer {
  case class Session(descType: String, sdp: String)

  enum Message {
    case Register(uid: Uid)
    case Announce(topic: String, descriptors: Set[ChannelConnectDescriptor])
    case LookupPeer(requestId: Uid, uid: Uid)
    case PeerInfo(requestId: Uid, uid: Uid, topics: Map[String, Set[ChannelConnectDescriptor]])
    case LookupTopic(requestId: Uid, topic: String)
    case TopicInfo(requestId: Uid, topic: String, peers: Map[Uid, Set[ChannelConnectDescriptor]])
    case Offer(from: Uid, to: Uid, session: Session)
    case Answer(from: Uid, to: Uid, session: Session)
  }
}

class SignalingServer(
    debug: Boolean
) {
  private val abort = Abort()

  private val clientsByUid       = mutable.Map.empty[Uid, Connection[Message]]
  private val uidByConn          = mutable.Map.empty[Connection[Message], Uid]
  private val announcementsByUid = mutable.Map.empty[Uid, Map[String, Set[channels.ChannelConnectDescriptor]]]

  private def log(msg: => String): Unit = if debug then println(s"[signaling] $msg")

  def stop(): Unit = abort.abort()

  def addIncomingConnection(latent: LatentConnection[Message]): Unit =
    latent.prepare(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def peerTopics(uid: Uid): Map[String, Set[channels.ChannelConnectDescriptor]] =
    announcementsByUid.getOrElse(uid, Map.empty)

  def topicPeers(topic: String): Map[Uid, Set[channels.ChannelConnectDescriptor]] =
    announcementsByUid.iterator.collect {
      case (uid, topics) if topics.contains(topic) => uid -> topics(topic)
    }.toMap

  private def disconnect(conn: Connection[Message]): Unit = {
    uidByConn.remove(conn).foreach { uid =>
      clientsByUid.remove(uid)
      announcementsByUid.remove(uid)
      log(s"disconnect ${Uid.unwrap(uid)}")
    }
    conn.close()
  }

  private def register(uid: Uid, conn: Connection[Message]): Unit = {
    uidByConn.get(conn).filter(_ != uid).foreach { previous =>
      clientsByUid.remove(previous)
      announcementsByUid.remove(previous)
    }
    clientsByUid.get(uid).filterNot(_ == conn).foreach(disconnect)
    uidByConn.update(conn, uid)
    clientsByUid.update(uid, conn)
    log(s"register ${Uid.unwrap(uid)}")
  }

  private def receive: Receive[Message] =
    (conn: Connection[Message]) => {
      {
        case Success(Message.Register(uid)) =>
          register(uid, conn)

        case Success(Message.Announce(topic, descriptors)) =>
          uidByConn.get(conn).foreach { uid =>
            announcementsByUid.update(uid, announcementsByUid.getOrElse(uid, Map.empty).updated(topic, descriptors))
            log(s"announce uid=${Uid.unwrap(uid)} topic=$topic descriptors=${descriptors.size}")
          }

        case Success(Message.LookupPeer(requestId, uid)) =>
          conn.send(Message.PeerInfo(requestId, uid, peerTopics(uid))).run(_ => ())

        case Success(Message.LookupTopic(requestId, topic)) =>
          conn.send(Message.TopicInfo(requestId, topic, topicPeers(topic))).run(_ => ())

        case Success(msg @ Message.Offer(from, to, _)) if uidByConn.get(conn).contains(from) =>
          clientsByUid.get(to).foreach(_.send(msg).run(_ => ()))
          log(s"offer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

        case Success(msg @ Message.Answer(from, to, _)) if uidByConn.get(conn).contains(from) =>
          clientsByUid.get(to).foreach(_.send(msg).run(_ => ()))
          log(s"answer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

        case Success(_) => ()

        case Failure(_) =>
          disconnect(conn)
      }
    }
}
