package webapps.ex2022reform.utils

import webapps.ex2022reform.services.{ToastMode, ToastType}
import webapps.ex2022reform.{JSImplicits, given_ExecutionContext}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object FutureExtensions {

  extension [T](self: Future[T]) {

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
}

object TryExtensions {
  extension [T](self: Try[T]) {
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
