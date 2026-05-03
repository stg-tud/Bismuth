package replication.research

import channels.{Abort, ChannelConnectDescriptor, ChannelResolver, Connection}
import de.rmgk.delay.{Async, Sync}
import rdts.base.Uid
import replication.research.SignalingServer.{Message, Session}

import scala.collection.mutable
import scala.util.{Failure, Success}

class SignalingClient(
    server: ChannelConnectDescriptor,
    resolver: ChannelResolver[Message],
    localUid: Uid,
    initialAnnouncements: Map[String, Set[ChannelConnectDescriptor]],
    onRegistered: () => Unit = () => (),
    onPeerInfo: (Uid, Map[String, Set[ChannelConnectDescriptor]]) => Unit =
      (_: Uid, _: Map[String, Set[ChannelConnectDescriptor]]) => (),
    onTopicInfo: (String, Map[Uid, Set[ChannelConnectDescriptor]]) => Unit =
      (_: String, _: Map[Uid, Set[ChannelConnectDescriptor]]) => (),
    onOffer: (Uid, Session) => Unit = (_: Uid, _: Session) => (),
    onAnswer: (Uid, Session) => Unit = (_: Uid, _: Session) => (),
    onDisconnected: () => Unit = () => (),
) {
  private val abort                                   = Abort()
  private var connection: Option[Connection[Message]] = None
  private val announcements                           = mutable.LinkedHashMap.from(initialAnnouncements)

  private def send(message: Message): Async[Any, Unit] =
    connection match
        case Some(conn) => conn.send(message)
        case None => Sync(throw IllegalStateException(s"signaling client ${Uid.unwrap(localUid)} is not connected"))

  def start(): Unit =
    resolver.connect(server, s"signal-${Uid.unwrap(localUid)}").foreach {
      _.prepare { _ =>
        {
          case Success(Message.Register(_))                                 => ()
          case Success(Message.PeerInfo(_, uid, topics))                    => onPeerInfo(uid, topics)
          case Success(Message.TopicInfo(_, topic, peers))                  => onTopicInfo(topic, peers)
          case Success(Message.Offer(from, to, session)) if to == localUid  => onOffer(from, session)
          case Success(Message.Answer(from, to, session)) if to == localUid => onAnswer(from, session)
          case Success(_)                                                   => ()
          case Failure(_)                                                   =>
            connection = None
            onDisconnected()
        }
      }.runIn(abort) {
        case Success(conn) =>
          connection = Some(conn)
          send(Message.Register(localUid)).run {
            case Success(_) =>
              announcements.foreach { case (topic, descriptors) =>
                send(Message.Announce(topic, descriptors)).run(_ => ())
              }
              onRegistered()
            case Failure(err) =>
              connection = None
              conn.close()
              err.printStackTrace()
          }
        case Failure(err) => err.printStackTrace()
      }
    }

  def stop(): Unit = {
    connection.foreach(_.close())
    connection = None
    abort.abort()
  }

  def announce(topic: String, descriptors: Set[ChannelConnectDescriptor]): Async[Any, Unit] = {
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
