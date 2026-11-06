package channels

import channels.connection.{ConnectionClosedException, NoMoreDataException}
import de.rmgk.delay.Callback

import java.io.IOException
import scala.util.{Failure, Success}

object TestUtil {

  def printErrors[T](cb: T => Unit): Callback[T] =
      case Success(mb) => cb(mb)
      case Failure(ex) => ex match
            case ex: IOException
                if ex.getCause != null && ex.getCause.getClass.getName == "java.lang.InterruptedException" =>
            case ex if ex.getClass.getName == "java.nio.channels.ClosedChannelException" =>
            case ex: NoMoreDataException                                                 =>
            case ex: ConnectionClosedException                                           =>
            case ex: IOException if ex.getMessage == "Socket closed"                     =>
            case ex: IOException if ex.getMessage == "closed"                            =>
            case ex: java.io.EOFException                                                =>
            case ex                                                                      => ex.printStackTrace()
}

extension (io: BroadcastIO[?]) {

  /** Fire all currently pending timers of a node using [[Timers.Manual]] (the default). */
  def fireTimers(): Int = io.timers match
      case manual: Timers.Manual => manual.fireAll()
      case other                 => throw IllegalStateException(s"timers are not manual: $other")
}

/** A connection that is only used as an identity in overlay state machine tests. */
final class StubConnection(name: String) extends channels.connection.Connection {
  override def send(message: channels.connection.MessageBuffer): de.rmgk.delay.Async[Any, Unit] =
    de.rmgk.delay.Async(())
  override def close(): Unit    = ()
  override def toString: String = s"StubConnection($name)"
}
