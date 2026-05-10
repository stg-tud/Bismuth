package channels

import de.rmgk.delay.Callback

import java.io.IOException
import scala.util.{Failure, Success}

object TestUtil {

  def printErrors[T](cb: T => Unit): Callback[T] =
      case Success(mb) => cb(mb)
      case Failure(ex) => ex match
            case ex: IOException if ex.getCause != null && ex.getCause.getClass.getName == "java.lang.InterruptedException" =>
            case ex if ex.getClass.getName == "java.nio.channels.ClosedChannelException"                                      =>
            case ex: NoMoreDataException                                                                                       =>
            case ex: ConnectionClosedException                                                                                 =>
            case ex: IOException if ex.getMessage == "Socket closed"                                                          =>
            case ex: java.io.EOFException                                                                                      =>
            case ex                                                                                                            => ex.printStackTrace()
}
