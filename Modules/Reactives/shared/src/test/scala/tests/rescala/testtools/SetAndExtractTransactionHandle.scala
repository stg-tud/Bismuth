package tests.rescala.testtools

import reactives.SelectedScheduler.candidate.State
import reactives.core.{Initializer, Scheduler}
import reactives.operator.Source

class SetAndExtractTransactionHandle(val api: reactives.default.type) {
  import api.*
  def SetAndExtractTransactionHandle[A, N](
      source: Source[A] { type State[V] = reactives.SelectedScheduler.State[V] },
      value: A
  )(using
      engine: Scheduler[State]
  ): Initializer[State] = {
    engine.forceNewTransaction(source) { implicit t =>
      source.admit(value)
      t.tx.initializer
    }
  }
}
