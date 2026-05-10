package benchmarks.b2018reactives

import org.openjdk.jmh.annotations.*

@State(Scope.Thread)
class Size {
  @Param(Array("2"))
  var size: Int = scala.compiletime.uninitialized
}
