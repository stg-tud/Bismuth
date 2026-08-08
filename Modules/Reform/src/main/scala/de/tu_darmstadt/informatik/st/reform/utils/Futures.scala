package de.tu_darmstadt.informatik.st.reform.utils

import de.tu_darmstadt.informatik.st.reform.services.{ToastMode, ToastType}
import de.tu_darmstadt.informatik.st.reform.{JSImplicits, given_ExecutionContext}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Futures {

  implicit class FutureOps[T](self: Future[T]) {

    def toastOnError(using
        jsImplicits: JSImplicits,
    )(mode: ToastMode = ToastMode.Short, style: ToastType = ToastType.Error): Unit =
      self
        .onComplete { value =>
          if value.isFailure then {
            value.failed.get.printStackTrace()
            jsImplicits.toaster.make(value.failed.get.getMessage.nn, mode, style)
          }
        }
  }

  implicit class TryOps[T](self: Try[T]) {
    def toastOnError(mode: ToastMode = ToastMode.Short, style: ToastType = ToastType.Error)(using
        jsImplicits: JSImplicits,
    ): Unit =
      self match {
        case Success(value)     =>
        case Failure(exception) =>
          exception.printStackTrace()
          jsImplicits.toaster.make(exception.getMessage.nn, mode, style)
      }
  }
}
