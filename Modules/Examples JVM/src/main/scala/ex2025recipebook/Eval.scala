package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.Historized.MetaDelta
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, LastWriterWins, MultiVersionRegister, ObserveRemoveMap}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalState {

  @Param(Array("1", "10"))
//  @Param(Array("1", "2", "5", "10"))
//  @Param(Array("1", "2", "5", "10", "25", "50", "100", "250", "500", "1000"))
//  @Param(Array("1", "2", "5", "10", "25", "50", "100", "250", "500", "1000", "2500", "5000", "10000"))
  var numOperations: Int   = 0
  val random               = new scala.util.Random(123456789)
  var randomArr: List[Int] = List.empty

  var mapSize: Int = 4

  @Setup(Level.Trial)
  def setup(): Unit =
    randomArr = List.fill(numOperations)(random.nextInt())

}

@State(Scope.Thread)
@AuxCounters(AuxCounters.Type.EVENTS)
class ResultCapture {
  var bufferSize: Int = 0

  def recordBufferSize(size: Int): Unit =
    bufferSize = size
}

@Fork(value = 1, warmups = 0)
@Warmup(iterations = 2)
@Measurement(iterations = 2)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DeltaBufferBenchmark {

  private inline def modReplica[A: {Bottom, Lattice}, B <: DeltaBuffer[A, B], L](
      deltaBuffer: DeltaBuffer[A, B],
      blackhole: Blackhole,
      list: List[L],
      resultCapture: ResultCapture,
      f: (A, L, LocalUid) => A
  ): Unit = {
    val replica = Replica(deltaBuffer)

    blackhole.consume(list.foreach { item =>
      replica.mod(a => f(a, item, replica.replicaId))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def baselineBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[LastWriterWins[Int]](List.empty[MetaDelta[LastWriterWins[Int]]])

    modReplica(deltaBuffer, blackhole, state.randomArr, resultCapture, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def nonRedundantBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   =
      DeltaBufferNonRedundant[LastWriterWins[Int]](List.empty[MetaDelta[LastWriterWins[Int]]], Dots.empty)

    modReplica(deltaBuffer, blackhole, state.randomArr, resultCapture, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def subsumedBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[LastWriterWins[Int]](List.empty[MetaDelta[LastWriterWins[Int]]])

    modReplica(deltaBuffer, blackhole, state.randomArr, resultCapture, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def baselineBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[GrowOnlyCounter](List.empty[MetaDelta[GrowOnlyCounter]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (goCounter, r, replicaID) => goCounter.inc()(using replicaID)
    )
  }

  @Benchmark
  def nonRedundantBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[GrowOnlyCounter](List.empty[MetaDelta[GrowOnlyCounter]], Dots.empty)

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (goCounter, r, replicaID) => goCounter.inc()(using replicaID)
    )
  }

  @Benchmark
  def subsumedBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[GrowOnlyCounter](List.empty[MetaDelta[GrowOnlyCounter]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (goCounter, r, replicaID) => goCounter.inc()(using replicaID)
    )
  }

  @Benchmark
  def baselineBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[EnableWinsFlag](List.empty[MetaDelta[EnableWinsFlag]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (ew, r, replicaId) => if r % 2 != 0 then ew.enable(using replicaId)() else ew.disable()
    )
  }

  @Benchmark
  def nonRedundantBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[EnableWinsFlag](List.empty[MetaDelta[EnableWinsFlag]], Dots.empty)

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (ew, r, replicaId) => if r % 2 != 0 then ew.enable(using replicaId)() else ew.disable()
    )
  }

  @Benchmark
  def subsumedBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[EnableWinsFlag](List.empty[MetaDelta[EnableWinsFlag]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (ew, r, replicaId) => if (r % 2) != 0 then ew.enable(using replicaId)() else ew.disable()
    )
  }

  @Benchmark
  def baselineBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[MultiVersionRegister[Int]](List.empty[MetaDelta[MultiVersionRegister[Int]]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (mvRegister, r, replicaId) => mvRegister.write(r)(using replicaId)
    )
  }

  @Benchmark
  def nonRedundantBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer =
      DeltaBufferNonRedundant[MultiVersionRegister[Int]](List.empty[MetaDelta[MultiVersionRegister[Int]]], Dots.empty)

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (mvRegister, r, replicaId) => mvRegister.write(r)(using replicaId)
    )
  }

  @Benchmark
  def subsumedBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[MultiVersionRegister[Int]](List.empty[MetaDelta[MultiVersionRegister[Int]]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (mvRegister, r, replicaId) => mvRegister.write(r)(using replicaId)
    )
  }

  @Benchmark
  def baselineBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ObserveRemoveMap[Int, Int]](List.empty[MetaDelta[ObserveRemoveMap[Int, Int]]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (orMap, r, replicaId) => orMap.update(r % state.mapSize, r)(using replicaId)
    )
  }

  @Benchmark
  def nonRedundantBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ObserveRemoveMap[Int, Int]](List.empty[MetaDelta[ObserveRemoveMap[Int, Int]]], Dots.empty)

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (orMap, r, replicaId) => orMap.update(r % state.mapSize, r)(using replicaId)
    )
  }

  @Benchmark
  def subsumedBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, Int]](List.empty[MetaDelta[ObserveRemoveMap[Int, Int]]])

    modReplica(
      deltaBuffer,
      blackhole,
      state.randomArr,
      resultCapture,
      (orMap, r, replicaId) => orMap.update(r % state.mapSize, r)(using replicaId)
    )
  }

}

object Eval {}
