package replication.research

import channels.{Abort, ArrayMessageBuffer, ConnectionDescriptor, ChannelResolver, Connection, Receive}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import de.rmgk.delay.Async
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.research.SignalingServer.{Message, Session}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Failure, Success}

class SignalingClient(
    server: ConnectionDescriptor,
    resolver: ChannelResolver,
    localUid: Uid,
    abort: Abort,
    webrtcAnswerer: Option[(Uid, Session) => Async[Any, Session]] = None,
    debug: Boolean = false,
)(using ec: ExecutionContext = ExecutionContext.global) {
  private var connection: Option[Connection]          = None
  private var connecting: Option[Promise[Connection]] = None
  private val pendingAnnouncements = mutable.HashMap.empty[Uid, Promise[Map[Uid, Set[ConnectionDescriptor]]]]
  private val pendingOffers        = mutable.HashMap.empty[Uid, Promise[Session]]

  private def send(conn: Connection, message: Message): Async[Any, Unit] =
    conn.send(ArrayMessageBuffer(writeToArray(message)))

  private def failPending(err: Throwable): Unit = synchronized {
    pendingAnnouncements.values.foreach(_.tryFailure(err))
    pendingOffers.values.foreach(_.tryFailure(err))
    pendingAnnouncements.clear()
    pendingOffers.clear()
  }

  private val receive: Receive = (_: Connection) => {
    case Success(buffer) =>
      readFromArray[Message](buffer.asArray) match
          case Message.TopicInfo(requestId, _, peers) =>
            synchronized {
              pendingAnnouncements.remove(requestId).foreach(_.trySuccess(peers))
            }
          case Message.Offer(from, to, session) if to == localUid =>
            webrtcAnswerer.foreach { answerer =>
              answerer(from, session).run {
                case Success(answer) =>
                  ensureConnected().run {
                    case Success(conn) => send(conn, Message.Answer(localUid, from, answer)).run(_ => ())
                    case Failure(_)    => ()
                  }
                case Failure(_) => ()
              }
            }
          case Message.Answer(from, to, session) if to == localUid =>
            synchronized {
              pendingOffers.remove(from).foreach(_.trySuccess(session))
            }
          case _ => ()
    case Failure(err) =>
      synchronized {
        connection = None
        connecting = None
      }
      failPending(err)
  }

  private def ensureConnected(): Async[Any, Connection] = Async.fromCallback {
    synchronized {
      connection match
          case Some(conn) => Async.handler.succeed(conn)
          case None       =>
            connecting match
                case Some(promise) => promise.future.onComplete(Async.handler.complete)
                case None          =>
                  val promise = Promise[Connection]()
                  connecting = Some(promise)
                  promise.future.onComplete(Async.handler.complete)
                  resolver.connect(server) match
                      case Some(latent) =>
                        latent.prepare(receive).runIn(abort) {
                          case Success(conn) =>
                            synchronized {
                              connection = Some(conn)
                            }
                            promise.trySuccess(conn): Unit
                          case Failure(err) =>
                            synchronized {
                              connecting = None
                            }
                            promise.tryFailure(err): Unit
                        }
                      case None =>
                        connecting = None
                        promise.tryFailure(
                          IllegalArgumentException(s"Could not resolve signaling server: $server")
                        ): Unit
    }
  }

  def announce(
      topic: String,
      descriptors: Set[ConnectionDescriptor],
      count: Int = Int.MaxValue,
  ): Async[Any, Map[Uid, Set[ConnectionDescriptor]]] =
    Async.fromCallback {
      ensureConnected().run {
        case Success(conn) =>
          val requestId = Uid.gen()
          val promise   = Promise[Map[Uid, Set[ConnectionDescriptor]]]()
          synchronized {
            pendingAnnouncements.update(requestId, promise)
          }
          promise.future.onComplete(Async.handler.complete)
          send(conn, Message.Announce(localUid, requestId, topic, descriptors, count)).run {
            case Success(_)   => ()
            case Failure(err) =>
              synchronized {
                pendingAnnouncements.remove(requestId)
              }
              promise.tryFailure(err): Unit
          }
        case Failure(err) => Async.handler.fail(err)
      }
    }

  def requestSession(to: Uid, offer: Session): Async[Any, Session] =
    Async.fromCallback {
      ensureConnected().run {
        case Success(conn) =>
          val promise = Promise[Session]()
          synchronized {
            pendingOffers.update(to, promise)
          }
          promise.future.onComplete(Async.handler.complete)
          send(conn, Message.Offer(localUid, to, offer)).run {
            case Success(_)   => ()
            case Failure(err) =>
              synchronized {
                pendingOffers.remove(to)
              }
              promise.tryFailure(err): Unit
          }
        case Failure(err) => Async.handler.fail(err)
      }
    }

  def isConnected: Boolean = synchronized(connection.nonEmpty)
}
