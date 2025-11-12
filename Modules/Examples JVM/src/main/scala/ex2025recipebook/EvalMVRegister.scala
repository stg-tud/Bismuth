package ex2025recipebook

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Warmup}
import org.openjdk.jmh.infra.Blackhole
import rdts.datatypes.MultiVersionRegister

import java.util.concurrent.TimeUnit


@Fork(value = 1, warmups = 0)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class BenchmarkMVRegister {

  @Benchmark
  def benchmarkMultiValueRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[MultiVersionRegister[Int]]()
    val replica = Replica(state.localUid, MultiVersionRegister.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => a.write(item))
      resultCapture.recordBufferSize(replica.buffer.getSize) // O(1) for size lookup in a set
    })
  }

  @Benchmark
  def benchmarkMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[MVRegister[Int]]()
    val replica = Replica(state.localUid, MVRegister.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => a.write(item))
      resultCapture.recordBufferSize(replica.buffer.getSize) // O(1) for size lookup in a set
    })
  }

}
