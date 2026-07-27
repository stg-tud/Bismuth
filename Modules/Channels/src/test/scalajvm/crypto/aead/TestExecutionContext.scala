package crypto.aead

import scala.concurrent.ExecutionContext

object TestExecutionContext {
  given executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
}
