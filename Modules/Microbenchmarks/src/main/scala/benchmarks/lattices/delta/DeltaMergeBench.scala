package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations.*
import rdts.base.Lattice
import rdts.base.LocalUid.asId
import rdts.datatypes.ReplicatedList
import rdts.dotted.Dotted
import rdts.time.{Dot, Dots}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@annotations.State(Scope.Thread)
class DeltaMergeBench {

  @Param(Array("1", "10", "100", "1000"))
  var size: Long = scala.compiletime.uninitialized

  var fullState: ReplicatedList[Long]         = scala.compiletime.uninitialized
  var plusOneState: ReplicatedList[Long]      = scala.compiletime.uninitialized
  var plusOneDeltaState: ReplicatedList[Long] = scala.compiletime.uninitialized

  def makeCContext(replicaID: String): Dots = {
    val dots = (0L until size).map(Dot(replicaID.asId.uid, _)).toSet
    Dots.from(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = ReplicatedList.empty[Long]

    val deltaState = baseState.insertAll(using "".asId)(0, 0L to size)
    fullState = Lattice.merge(baseState, deltaState)

    plusOneDeltaState = fullState.insert(using "".asId)(0, size)
    plusOneState = Lattice.merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: Dotted[ReplicatedList[Long]] = {
    Dotted(Lattice.merge(fullState, plusOneState))
  }

  @Benchmark
  def fullDiff: Option[ReplicatedList[Long]] = {
    Lattice.diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: ReplicatedList[Long] = {
    Lattice.diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        Lattice.merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: ReplicatedList[Long] = {
    Lattice.merge(fullState, plusOneDeltaState)
  }
}
