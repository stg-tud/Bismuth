package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, GrowOnlySet, LastWriterWins, ObserveRemoveMap, PosNegCounter, ReplicatedSet}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalState {

//  @Param(Array("1", "10", "100", "200", "500", "1000", "2000", "5000", "10000"))
  @Param(Array("1", "10", "100", "1000", "10000"))
  var numOperations: Int     = 0
  val random                 = new scala.util.Random(123456789)
  var randomArr: List[Int]   = List.empty
  val localUid: LocalUid     = LocalUid.gen()
  val foreignUid: LocalUid   = LocalUid.gen()
  val localReplicaShare: Int = 9

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
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DeltaBufferBenchmark {

  @Benchmark
  def baselineBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[LastWriterWins[Int]]()

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def nonRedundantBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[LastWriterWins[Int]]()

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def subsumedBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[LastWriterWins[Int]]()

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r, _) => lww.write(r))
  }

  @Benchmark
  def baselineBufferGCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r, localUid) => goCounter.inc()(using localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferGCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r, localUid) => goCounter.inc()(using localUid)
    )
  }

  @Benchmark
  def subsumedBufferGCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r, localUid) => goCounter.inc()(using localUid)
    )
  }

  @Benchmark
  def baselineBufferPNCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[PosNegCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      PosNegCounter.zero,
      (goCounter, r, localUid) =>
        if r % 2 == 0 then goCounter.inc()(using localUid) else goCounter.dec()(using localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferPNCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[PosNegCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      PosNegCounter.zero,
      (goCounter, r, localUid) =>
        if r % 2 == 0 then goCounter.inc()(using localUid) else goCounter.dec()(using localUid)
    )
  }

  @Benchmark
  def subsumedBufferPNCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[PosNegCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      PosNegCounter.zero,
      (goCounter, r, localUid) =>
        if r % 2 == 0 then goCounter.inc()(using localUid) else goCounter.dec()(using localUid)
    )
  }

  @Benchmark
  def baselineBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[EnableWinsFlag]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      EnableWinsFlag.empty,
      (ew, r, localUid) => if r % 2 != 0 then ew.enable(using localUid)() else ew.disable()
    )
  }

  @Benchmark
  def nonRedundantBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[EnableWinsFlag]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      EnableWinsFlag.empty,
      (ew, r, localUid) => if r % 2 != 0 then ew.enable(using localUid)() else ew.disable()
    )
  }

  @Benchmark
  def subsumedBufferEWFlag(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[EnableWinsFlag]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      EnableWinsFlag.empty,
      (ew, r, localUid) => if r % 2 != 0 then ew.enable(using localUid)() else ew.disable()
    )
  }

  @Benchmark
  def baselineBufferGSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]               = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer = DeltaBufferEverything[GrowOnlySet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r)
    )
  }

  @Benchmark
  def nonRedundantBufferGSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]               = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer = DeltaBufferNonRedundant[GrowOnlySet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r)
    )
  }

  @Benchmark
  def subsumedBufferGSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]               = Bottom.provide(0)
    given Lattice[GrowOnlySet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[GrowOnlySet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlySet.empty[Int],
      (gSet, r, _) => gSet.add(r)
    )
  }

  @Benchmark
  def baselineBufferORSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                 = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ReplicatedSet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r)
    )
  }

  @Benchmark
  def nonRedundantBufferORSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                 = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ReplicatedSet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r)
    )
  }

  @Benchmark
  def subsumedBufferORSet(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                 = Bottom.provide(0)
    given Lattice[ReplicatedSet[Int]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ReplicatedSet[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ReplicatedSet.empty[Int],
      (orSet, r, localUid) => Eval.performORSetOperation(orSet, localUid, r)
    )
  }

  @Benchmark
  def baselineBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer = DeltaBufferEverything[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r)
    )
  }

  @Benchmark
  def nonRedundantBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r)
    )
  }

  @Benchmark
  def subsumedBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
        given Lattice[Int] = math.max
        Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, LastWriterWins[Int]]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, LastWriterWins[Int]],
      (orMap, r, localUid) => Eval.performORMapOperationLWW(orMap, localUid, r)
    )
  }

  @Benchmark
  def baselineBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer    = DeltaBufferEverything[NestedKeepRemoveList[Int]]()
    val localReplica   = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer    = DeltaBufferNonRedundant[NestedKeepRemoveList[Int]]()
    val localReplica   = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer    = DeltaBufferSubsumed[NestedKeepRemoveList[Int]]()
    val localReplica   = Replica(state.localUid, NestedKeepRemoveList.empty[Int], deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, NestedKeepRemoveList.empty[Int], deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performKRListOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performKRListOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def baselineBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer    = DeltaBufferEverything[RecipeBook]()
    val localReplica   = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer    = DeltaBufferNonRedundant[RecipeBook]()
    val localReplica   = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer    = DeltaBufferSubsumed[RecipeBook]()
    val localReplica   = Replica(state.localUid, RecipeBook.empty, deltaBuffer)
    val foreignReplica = Replica(state.foreignUid, RecipeBook.empty, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      if math.abs(item % 10) < state.localReplicaShare then {
        localReplica.mod(a => Eval.performRecipeBookOperation(a, item, localReplica.replicaId))
      } else {
        val delta = foreignReplica.produceDelta(a => Eval.performRecipeBookOperation(a, item, foreignReplica.replicaId))
        localReplica.applyDelta(delta)
      }
      resultCapture.recordBufferSize(localReplica.buffer.getSize)
    })
  }

}

object Eval {

  inline def modReplica[A: {Lattice}, B <: DeltaBuffer[A, B]](
      deltaBuffer: DeltaBuffer[A, B],
      blackhole: Blackhole,
      state: EvalState,
      resultCapture: ResultCapture,
      initialState: A,
      f: (A, Int, LocalUid) => A
  ): Unit = {
    val localReplica   = Replica(state.localUid, initialState, deltaBuffer)
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

  private def lottery(keys: List[String], random: Int): String = {
    val randomIndex = math.abs(random % keys.size)
    keys(randomIndex)
  }

  def performRecipeBookOperation(recipeBook: RecipeBook, random: Int, replicaID: LocalUid): RecipeBook = {
    def addIngredient(): RecipeBook = {
      val randomRecipeKey = lottery(recipeBook.keys.toList, random)
      recipeBook.addIngredient(randomRecipeKey, Ingredient(random.toString, random.toDouble, random.toString))(using
        replicaID
      )
    }

    math.abs(random % 10) match {
      case 0 if recipeBook.nonEmpty =>
        val randomKey = lottery(recipeBook.keys.toList, random)
        recipeBook.deleteRecipe(randomKey)
      case 1 if recipeBook.nonEmpty =>
        val randomKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateRecipeTitle(randomKey, random.toString)(using replicaID)
      case 2 if recipeBook.nonEmpty => addIngredient()
      case 3 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        val ingredientsSize = recipeBook.get(randomRecipeKey).get.ingredients.size
        if ingredientsSize > 0 then {
          val randomIngredientIndex = random % ingredientsSize
          recipeBook.updateIngredient(
            randomRecipeKey,
            randomIngredientIndex,
            _ => Ingredient(random.toString, random.toDouble, random.toString)
          )(using replicaID)
        } else addIngredient()
      case 4 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        val ingredientsSize = recipeBook.get(randomRecipeKey).get.ingredients.size
        if ingredientsSize > 0 then {
          val randomIngredientIndex = random % ingredientsSize
          recipeBook.deleteIngredient(randomRecipeKey, randomIngredientIndex)(using replicaID)
        } else addIngredient()
      case 5 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateServings(randomRecipeKey, random)(using replicaID)
      case 6 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateCookingTime(randomRecipeKey, random)(using replicaID)
      case 7 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateDescription(randomRecipeKey, random.toString)(using replicaID)
      case 8 if recipeBook.nonEmpty =>
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateFavorite(randomRecipeKey, random % 2 == 0)(using replicaID)
      case _ =>
        val recipe = Recipe.empty
        recipeBook.addRecipe(random.toString, recipe)(using replicaID)
    }
  }

  def performKRListOperation(
      krList: NestedKeepRemoveList[Int],
      random: Int,
      replicaID: LocalUid
  ): NestedKeepRemoveList[Int] = {
    if krList.size == 0 then return krList.insertAt(0, random)(using replicaID)
    math.abs(random % 3) match {
      case 0 =>
        val index = math.abs(random % krList.size)
        krList.remove(index)
      case 1 =>
        val index = math.abs(random % krList.size)
        krList.update(index, _ => random)(using replicaID)
      case _ =>
        val index = math.abs(random % krList.size)
        krList.insertAt(index, random)(using replicaID)
    }
  }

  def initializeRecipeBook(localUid: LocalUid, num: Int): RecipeBook = {
    var recipeBook: RecipeBook = RecipeBook.empty

    (0 to num).foreach(i =>
        val ingredients: Iterable[Ingredient] = (0 to i % 10).map(j => Ingredient(j.toString, j, j.toString))
        val recipe: Recipe                    = Recipe(i.toString, ingredients)(using localUid)
        recipeBook = recipeBook `merge` recipeBook.addRecipe(i.toString, recipe)(using localUid)
    )

    recipeBook
  }

  def performORSetOperation(orSet: ReplicatedSet[Int], localUid: LocalUid, random: Int): ReplicatedSet[Int] = {
    if orSet.size == 0 then return orSet.add(using localUid)(random)
    math.abs(random % 2) match {
      case 0 => orSet.add(using localUid)(random)
      case 1 =>
        val index = math.abs(random % orSet.size)
        orSet.remove(index)
    }
  }

  def performORMapOperationLWW(orMap: ObserveRemoveMap[Int, LastWriterWins[Int]], localUid: LocalUid, random: Int): ObserveRemoveMap[Int, LastWriterWins[Int]] = {
    given Bottom[Int]= Bottom.provide(0)
    if orMap.entries.isEmpty then return orMap.update(random, LastWriterWins.empty[Int].write(random))(using localUid)
    math.abs(random % 2) match {
      case 0 => {
        val randomKey = orMap.entries.toList(math.abs(random % orMap.entries.size))._1
        orMap.remove(randomKey)
      }
      case _ => orMap.update(random, LastWriterWins.empty[Int].write(random))(using localUid)
    }
  }

}
