package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, GrowOnlySet, LastWriterWins, ObserveRemoveMap, ReplicatedSet}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalORMapState {

  var numOperations: Int   = 10000
  val random               = new scala.util.Random(123456789)
  var randomArr: List[Int] = List.empty
  val localUid: LocalUid   = LocalUid.gen()
  val foreignUid: LocalUid = LocalUid.gen()
  val localReplicaShare: Int = 9

  @Param(Array("1", "10", "100", "1000", "10000"))
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
  def baselineBufferGSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer = DeltaBufferEverything[GrowOnlySet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r % state.mapSize)
    )
  }

  @Benchmark
  def nonRedundantBufferGSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer = DeltaBufferNonRedundant[GrowOnlySet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r % state.mapSize)
    )
  }

  @Benchmark
  def subsumedBufferGSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[GrowOnlySet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r % state.mapSize)
    )
  }

  @Benchmark
  def baselineBufferORSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ReplicatedSet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def nonRedundantBufferORSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ReplicatedSet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def subsumedBufferORSet(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
      given Lattice[Int] = math.max
      Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ReplicatedSet[Int]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def baselineBufferORMapLWW(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, LastWriterWins[Int]]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer = DeltaBufferEverything[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def nonRedundantBufferORMapLWW(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, LastWriterWins[Int]]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer = DeltaBufferNonRedundant[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    EvalORMap.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def subsumedBufferORMapLWW(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
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
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r % state.mapSize)
    )
  }

  @Benchmark
  def baselineBufferKRList(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferEverything[NestedKeepRemoveList[Int]]()
    val localReplica = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferKRList(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferNonRedundant[NestedKeepRemoveList[Int]]()
    val localReplica = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferKRList(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferSubsumed[NestedKeepRemoveList[Int]]()
    val localReplica = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def baselineBufferRecipeBook(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[RecipeBook]()
    val localReplica = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferRecipeBook(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[RecipeBook]()
    val localReplica = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferRecipeBook(blackhole: Blackhole, state: EvalORMapState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[RecipeBook]()
    val localReplica = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item % state.mapSize, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item % state.mapSize, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

}

object EvalORMap {

  inline def modReplica[A: {Lattice}, B <: DeltaBuffer[A, B]](
      deltaBuffer: DeltaBuffer[A, B],
      blackhole: Blackhole,
      state: EvalORMapState,
      resultCapture: ResultCapture,
      initialState: A,
      f: (A, Int, LocalUid) => A
  ): Unit = {
    val localReplica = Replica(state.localUid, initialState, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, initialState, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => f(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => f(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize) // O(1) for size lookup in a set
    })
  }

  def performORMapOperationEWFlag(orMap: ObserveRemoveMap[Int, EnableWinsFlag], localUid: LocalUid, ewFlag: EnableWinsFlag, random: Int, mapSize: Int): ObserveRemoveMap[Int, EnableWinsFlag] = {
    given Bottom[Int]= Bottom.provide(0)
    if orMap.entries.isEmpty then return orMap.update(random, if random%2 == 0 then EnableWinsFlag.empty.enable(using localUid)() else EnableWinsFlag.empty.disable())(using localUid)
    math.abs(random % 2) match {
      case 0 => {
        val randomKey = orMap.entries.toList(math.abs(random % orMap.entries.size))._1
        orMap.remove(randomKey)
      }
      case _ => orMap.transform(random) {
        case Some(value) => if value.read then Option(value.disable()) else Option(value.enable(using localUid)())
        case None => Option(if random%2 == 0 then EnableWinsFlag.empty.enable(using localUid)() else EnableWinsFlag.empty.disable())
      }(using localUid)
    }
  }

}
