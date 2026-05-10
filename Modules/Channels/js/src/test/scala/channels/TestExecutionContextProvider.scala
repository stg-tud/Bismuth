package channels

import scala.concurrent.ExecutionContext

object TestExecutionContextProvider {
  def create(): ExecutionContext = ExecutionContext.global
}
