package benchmarks.b2018reactives

import org.openjdk.jmh.annotations.{Scope, State}

@State(Scope.Benchmark)
class EngineParam {
  var engineName: String             = reactives.SelectedScheduler.candidate.scheduler.schedulerName
  def engine: reactives.default.type = reactives.default
}
