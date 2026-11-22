package channels
package connection

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext

object ConcurrencyHelper {
  def makeExecutionContext(singleThreadExecutor: Boolean): ExecutionContext =
    if singleThreadExecutor then
        val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor { r =>
          val thread = new Thread(r)
          thread.setDaemon(true)
          thread
        }

        ExecutionContext.fromExecutorService(singleThreadExecutor)
    else
        MessagingIO.executeImmediately
}
