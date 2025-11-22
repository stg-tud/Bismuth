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
  def baselineBufferMultiValueRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferMultiValueRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

  @Benchmark
  def subsumedBufferMultiValueRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

  @Benchmark
  def baselineBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[MVRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MVRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[MVRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MVRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

  @Benchmark
  def subsumedBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[MVRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MVRegister.empty[Int],
      (mvRegister, r, localUid) => mvRegister.write(r)(using localUid)
    )
  }

}
