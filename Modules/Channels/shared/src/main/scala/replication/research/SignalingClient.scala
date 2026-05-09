package replication.research

import channels.{Abort, ArrayMessageBuffer, ChannelConnectInfo, ChannelResolver, Connection}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import de.rmgk.delay.{Async, Sync}
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.research.SignalingServer.{Message, Session}

import scala.collection.mutable
import scala.util.{Failure, Success}

class SignalingClient(
    server: ChannelConnectInfo,
    resolver: ChannelResolver,
    localUid: Uid,
    initialAnnouncements: Map[String, Set[ChannelConnectInfo]],
    debug: Boolean = false,
    onRegistered: () => Unit = () => (),
    onPeerInfo: (Uid, Map[String, Set[ChannelConnectInfo]]) => Unit =
      (_: Uid, _: Map[String, Set[ChannelConnectInfo]]) => (),
    onTopicInfo: (String, Map[Uid, Set[ChannelConnectInfo]]) => Unit =
      (_: String, _: Map[Uid, Set[ChannelConnectInfo]]) => (),
    onOffer: (Uid, Session) => Unit = (_: Uid, _: Session) => (),
    onAnswer: (Uid, Session) => Unit = (_: Uid, _: Session) => (),
    onDisconnected: () => Unit = () => (),
) {
  private def log(msg: => String): Unit = if debug then println(s"[signaling-client ${Uid.unwrap(localUid)}] $msg")

  private val abort                          = Abort()
  private var connection: Option[Connection] = None
  private val announcements                  = mutable.LinkedHashMap.from(initialAnnouncements)

  private def describe(conn: Connection): String =
    s"${conn.getClass.getSimpleName}@${System.identityHashCode(conn)} ${conn.info}"

  private def send(message: Message): Async[Any, Unit] =
    connection match
        case Some(conn) =>
          log(s"send $message via ${describe(conn)}")
          conn.send(ArrayMessageBuffer(writeToArray(message)))
        case None => Sync(throw IllegalStateException(s"signaling client ${Uid.unwrap(localUid)} is not connected"))

  def start(): Unit = {
    log(s"connecting to $server")
    resolver.connect(server).foreach {
      _.prepare { _ =>
        {
          case Success(buffer) =>
            val message = readFromArray[Message](buffer.asArray)
            log(s"received $message")
            message match
                case Message.Register(_)                                 =>
                  announcements.foreach { case (topic, descriptors) =>
                    log(s"announce topic=$topic descriptors=$descriptors")
                    send(Message.Announce(topic, descriptors)).run {
                      case Success(_)  => log(s"announce sent for topic=$topic")
                      case Failure(ex) => log(s"announce failed for topic=$topic: ${ex.getMessage}")
                    }
                  }
                  onRegistered()
                case Message.PeerInfo(_, uid, topics)                    => onPeerInfo(uid, topics)
                case Message.TopicInfo(_, topic, peers)                  => onTopicInfo(topic, peers)
                case Message.Offer(from, to, session) if to == localUid  => onOffer(from, session)
                case Message.Answer(from, to, session) if to == localUid => onAnswer(from, session)
                case _                                                   => ()
          case Failure(_) =>
            connection = None
            onDisconnected()
        }
      }.runIn(abort) {
        case Success(conn) =>
          connection = Some(conn)
          log(s"connected to signaling server $server via ${describe(conn)}")
          send(Message.Register(localUid)).run {
            case Success(_) => log("register sent")
            case Failure(err) =>
              connection = None
              conn.close()
              err.printStackTrace()
          }
        case Failure(err) => err.printStackTrace()
      }
    }
  }

  def stop(): Unit = {
    connection.foreach(_.close())
    connection = None
    abort.abort()
  }

  def announce(topic: String, descriptors: Set[ChannelConnectInfo]): Async[Any, Unit] = {
    announcements.update(topic, descriptors)
    send(Message.Announce(topic, descriptors))
  }

  def lookupPeer(uid: Uid): Async[Any, Unit] =
    send(Message.LookupPeer(Uid.gen(), uid))

  def lookupTopic(topic: String, count: Int = Int.MaxValue): Async[Any, Unit] =
    send(Message.LookupTopic(Uid.gen(), topic, count))

  def offer(to: Uid, session: Session): Async[Any, Unit] =
    send(Message.Offer(localUid, to, session))

  def answer(to: Uid, session: Session): Async[Any, Unit] =
    send(Message.Answer(localUid, to, session))

  def isConnected: Boolean = connection.nonEmpty
}
