package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, LastWriterWins, ObserveRemoveMap}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalORMapState {

  var numOperations: Int   = 1000
  val random               = new scala.util.Random(123456789)
  var randomArr: List[Int] = List.empty
  val localUid: LocalUid   = LocalUid.gen()

  @Param(Array("1", "10", "100", "1000"))
  var mapSize: Int = 0

  @Setup(Level.Trial)
  def setup(): Unit =
    randomArr = List.fill(numOperations)(random.nextInt())

}

@Fork(value = 1, warmups = 0)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DeltaBufferORMapBenchmark {

  @Benchmark
  def baselineBufferORMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ObserveRemoveMap[Int, Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r % state.mapSize, r)(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferORMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ObserveRemoveMap[Int, Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r % state.mapSize, r)(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferORMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r % state.mapSize, r)(using state.localUid)
    )
  }

  @Benchmark
  def baselineBufferORIMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, LastWriterWins[Int]]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r) => orMap.update(r % state.mapSize, LastWriterWins.empty.write(r))(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferORIMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, LastWriterWins[Int]]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r) => orMap.update(r % state.mapSize, LastWriterWins.empty.write(r))(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferORIMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, LastWriterWins[Int]]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r) => orMap.update(r % state.mapSize, LastWriterWins.empty.write(r))(using state.localUid)
    )
  }

  @Benchmark
  def baselineBufferORNMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, EnableWinsFlag]] = Lattice.derived
    val deltaBuffer = DeltaBufferEverything[ObserveRemoveMap[Int, EnableWinsFlag]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, EnableWinsFlag],
      (orMap, r) => orMap.transform(r) {
        case Some(ew) => Some(if (r % 2) != 0 then ew.enable(using state.localUid)() else ew.disable())
        case None => Some(if (r%2) != 0 then EnableWinsFlag.empty.enable(using state.localUid)() else EnableWinsFlag.empty)
      }(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferORNMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, EnableWinsFlag]] = Lattice.derived
    val deltaBuffer = DeltaBufferNonRedundant[ObserveRemoveMap[Int, EnableWinsFlag]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, EnableWinsFlag],
      (orMap, r) => orMap.transform(r) {
        case Some(ew) => Some(if (r % 2) != 0 then ew.enable(using state.localUid)() else ew.disable())
        case None => Some(if (r%2) != 0 then EnableWinsFlag.empty.enable(using state.localUid)() else EnableWinsFlag.empty)
      }(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferORNMap(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, EnableWinsFlag]] = Lattice.derived
    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, EnableWinsFlag]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, EnableWinsFlag],
      (orMap, r) => orMap.transform(r) {
        case Some(ew) => Some(if (r % 2) != 0 then ew.enable(using state.localUid)() else ew.disable())
        case None => Some(if (r%2) != 0 then EnableWinsFlag.empty.enable(using state.localUid)() else EnableWinsFlag.empty)
      }(using state.localUid)
    )
  }

}

object EvalORMap {

  inline def modReplica[A: {Lattice}, B <: DeltaBuffer[A, B]](
      deltaBuffer: DeltaBuffer[A, B],
      blackhole: Blackhole,
      state: EvalORMapState,
      resultCapture: ResultCapture,
      initialState: A,
      f: (A, Int) => A
  ): Unit = {
    val replica = Replica(state.localUid, initialState, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => f(a, item))
      resultCapture.recordBufferSize(replica.buffer.getSize) // O(1) for size lookup in a set
    })
  }

}
