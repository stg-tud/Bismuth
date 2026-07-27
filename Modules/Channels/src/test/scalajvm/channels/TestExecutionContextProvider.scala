package channels

import java.util.concurrent.{Executors, ThreadFactory}
import scala.concurrent.ExecutionContext

object TestExecutionContextProvider {
  def create(): ExecutionContext = {
    val threadFactory: ThreadFactory = r => {
      val thread = new Thread(r)
      thread.setDaemon(true)
      thread.setName("channels-test")
      thread
    }
    ExecutionContext.fromExecutorService(Executors.newCachedThreadPool(threadFactory))
  }
}
