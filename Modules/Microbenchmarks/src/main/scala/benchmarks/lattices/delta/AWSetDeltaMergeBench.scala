package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.base.{Lattice, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.time.{Dot, Dots}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@annotations.State(Scope.Thread)
class AWSetDeltaMergeBench {

  @Param(Array("1", "10", "100", "1000"))
  var size: Long = scala.compiletime.uninitialized

  var fullState: ReplicatedSet[Long]         = scala.compiletime.uninitialized
  var plusOneState: ReplicatedSet[Long]      = scala.compiletime.uninitialized
  var plusOneDeltaState: ReplicatedSet[Long] = scala.compiletime.uninitialized

  def makeCContext(replicaID: Uid): Dots = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    Dots.from(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = ReplicatedSet.empty[Long]

    val deltaState = baseState.addAll(using "".asId)(0L to size)
    fullState = Lattice.merge(baseState, deltaState)

    plusOneDeltaState = fullState.add(using "".asId)(size)
    plusOneState = Lattice.merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: ReplicatedSet[Long] = {
    Lattice.merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[ReplicatedSet[Long]] = {
    Lattice.diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: ReplicatedSet[Long] = {
    Lattice.diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        Lattice.merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: ReplicatedSet[Long] = {
    Lattice.merge(fullState, plusOneDeltaState)
  }
}
