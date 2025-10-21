package benchmarks.lattices

import org.openjdk.jmh.annotations.*
import rdts.base.{Lattice, Uid}
import rdts.time.{Dot, Dots}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ContextBench {

  @Param(Array("1", "1000"))
  var size: Long = scala.compiletime.uninitialized

  var rep1Set: Dots        = scala.compiletime.uninitialized
  var rep1SetPlusOne: Dots = scala.compiletime.uninitialized
  var rep2Set: Dots        = scala.compiletime.uninitialized
  val rep1id: Uid               = Uid.gen()
  val rep2id: Uid               = Uid.gen()
  var rep1single: Dots     = scala.compiletime.uninitialized

  private def makeRep(rep: Uid, mul: Long, off: Long, len: Long): Dots = {
    val ranges = Range.Long(0L, size, 1).map(i => Range.Long(i * mul + off, i * mul + len + off, 1))
    Dots.from(ranges.flatten.iterator.map(Dot(rep, _)).toSet)
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id, 10, 0, 7)
    rep2Set = makeRep(rep2id, 10, 5, 7)
    rep1SetPlusOne = rep1Set.add(rep2id, 5)
    rep1single = Dots.empty.add(rep1id, size + 10)
  }

  @Benchmark
  def merge(): Dots = Lattice.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeSelf(): Dots = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne(): Dots = Lattice.merge(rep1Set, rep1SetPlusOne)

  @Benchmark
  def diffSelf(): Dots = rep1Set.diff(rep1Set)

  @Benchmark
  def diffOther(): Dots = rep1Set.diff(rep2Set)

  @Benchmark
  def diffSingle(): Dots = rep1SetPlusOne.diff(rep1Set)

  @Benchmark
  def intersectSelf(): Dots = rep1Set.intersect(rep1Set)

  @Benchmark
  def intersectOther(): Dots = rep1Set.intersect(rep2Set)

  @Benchmark
  def containsOther(): Boolean = rep1Set.contains(rep2Set)

  @Benchmark
  def containsSelf(): Boolean = rep1Set.contains(rep1Set)

  @Benchmark
  def disjunctOther(): Boolean = rep1Set.disjunct(rep2Set)

  @Benchmark
  def disjunctSelf(): Boolean = rep1Set.disjunct(rep1Set)

}
