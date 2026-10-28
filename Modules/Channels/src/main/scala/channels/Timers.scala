package channels

import channels.broadcast.BroadcastTimer
import channels.overlay.OverlayController.OverlayTimer

import java.util.TimerTask
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Runs tasks after a delay. The state machines only name the purpose of a timer; the driver maps purposes to
  * delays ([[TimerDelays]]) and a [[Timers]] implementation decides what "later" means.
  */
trait Timers {
  def schedule(delay: FiniteDuration)(task: () => Unit): Unit
}

object Timers {

  /** Timers that only fire when explicitly told to. Delays are ignored.
    * For deterministic tests, and for applications that do not want any background activity.
    */
  final class Manual extends Timers {
    private var pending: Vector[() => Unit] = Vector.empty

    override def schedule(delay: FiniteDuration)(task: () => Unit): Unit = synchronized {
      pending = pending :+ task
    }

    def size: Int = synchronized(pending.size)

    /** Fires all timers scheduled so far, in scheduling order. Timers scheduled while firing stay pending.
      * @return the number of fired timers
      */
    def fireAll(): Int = {
      val due = synchronized {
        val current = pending
        pending = Vector.empty
        current
      }
      due.foreach(_())
      due.size
    }
  }

  /** Fires timers on the thread of a [[java.util.Timer]]. */
  final class OnJavaTimer(timer: java.util.Timer) extends Timers {
    override def schedule(delay: FiniteDuration)(task: () => Unit): Unit =
      timer.schedule(
        new TimerTask { override def run(): Unit = task() },
        delay.toMillis
      )
  }
}

/** Delays for each timer purpose. */
final case class TimerDelays(
    shuffle: FiniteDuration = 5.seconds,
    handshake: FiniteDuration = 10.seconds,
    repair: FiniteDuration = 1.second,
) {
  def apply(timer: OverlayTimer): FiniteDuration = timer match
      case OverlayTimer.Shuffle      => shuffle
      case OverlayTimer.Handshake(_) => handshake

  def apply(timer: BroadcastTimer): FiniteDuration = timer match
      case BroadcastTimer.Repair => repair
}
