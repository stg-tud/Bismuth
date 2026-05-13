package de.tu_darmstadt.informatik.st.reform.utils

import de.tu_darmstadt.informatik.st.reform.given_ExecutionContext
import reactives.core.Disconnectable
import reactives.default.*

import scala.concurrent.*

object Seqnal {

  implicit class SignalOps[T](self: Signal[T]) {

    def toFuture: Future[T] = {
      val promise                        = Promise[T]()
      val disconnectable: Disconnectable = self.observe { v =>
        promise.success(v)
      }
      promise.future.map { v =>
        disconnectable.disconnect()
        v
      }
    }

    def waitUntil(pred: T => Boolean): Future[T] = {
      val promise                        = Promise[T]()
      val disconnectable: Disconnectable = self.observe { v =>
        if pred(v) then {
          promise.success(v)
        }
      }
      promise.future.map { v =>
        disconnectable.disconnect()
        v
      }
    }
  }
}
