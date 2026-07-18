package channels

import channels.connection.Abort

import java.util.concurrent.{Executors, ThreadFactory}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future, Promise}

private def fixedThreadExecutionContext(threads: Int): ExecutionContextExecutorService = {
  val factory: ThreadFactory = r => {
    val thread = new Thread(r)
    thread.setDaemon(true)
    thread.setName(s"echo-test-$threads")
    thread
  }
  ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(threads, factory))
}

class NioTCPAbortTest extends munit.FunSuite {
  test("aborting nio selector loop stops it") {
    val ec    = fixedThreadExecutionContext(2)
    val nio   = new NioTCP()
    val abort = Abort()
    val done  = Promise[Unit]()

    ec.execute(() =>
      try {
        nio.loopSelection(abort)
        done.success(())
      } catch {
        case err: Throwable => done.failure(err)
      }
    )

    abort.abort()

    given ExecutionContext = ec
    done.future
      .flatMap(_ => Future.successful(assert(true)))
      .transform { result =>
        ec.shutdownNow()
        result
      }
  }
}

class NioTCPAbortRegressionTest extends munit.FunSuite {
  test("aborted nio selector loops free a two-thread execution context for later work") {
    val ec = fixedThreadExecutionContext(2)

    def startLoop(): (NioTCP, Abort, Future[Unit]) = {
      val nio   = new NioTCP()
      val abort = Abort()
      val done  = Promise[Unit]()
      ec.execute(() =>
        try {
          nio.loopSelection(abort)
          done.success(())
        } catch {
          case err: Throwable => done.failure(err)
        }
      )
      (nio, abort, done.future)
    }

    val (nio1, abort1, done1) = startLoop()
    val (nio2, abort2, done2) = startLoop()

    abort1.abort()
    abort2.abort()

    given ExecutionContext = ec
    val laterWork          = Promise[Unit]()

    done1
      .flatMap(_ => done2)
      .map { _ =>
        ec.execute(() => laterWork.success(()))
        ()
      }
      .flatMap(_ => laterWork.future)
      .transform { result =>
        ec.shutdownNow()
        result
      }
  }
}
