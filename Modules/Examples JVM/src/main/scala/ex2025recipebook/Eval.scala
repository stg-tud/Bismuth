package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, LastWriterWins, MultiVersionRegister, ObserveRemoveMap}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalState {

  @Param(Array("1", "2", "5", "10", "100", "200", "500", "1000", "2000", "5000", "10000"))
  var numOperations: Int   = 0
  val random               = new scala.util.Random(123456789)
  var randomArr: List[Int] = List.empty
  val localUid: LocalUid = LocalUid.gen()

  @Setup(Level.Trial)
  def setup(): Unit = {
    randomArr = List.fill(numOperations)(random.nextInt())
  }

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

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r) => lww.write(r))
  }

  @Benchmark
  def nonRedundantBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[LastWriterWins[Int]]()

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r) => lww.write(r))
  }

  @Benchmark
  def subsumedBufferLWW(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[LastWriterWins[Int]]()

    Eval.modReplica(deltaBuffer, blackhole, state, resultCapture, LastWriterWins.empty, (lww, r) => lww.write(r))
  }

  @Benchmark
  def baselineBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferEverything[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r) => goCounter.inc()(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferNonRedundant[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r) => goCounter.inc()(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferGOCounter(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int] = Bottom.provide(0)
    val deltaBuffer   = DeltaBufferSubsumed[GrowOnlyCounter]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      GrowOnlyCounter.zero,
      (goCounter, r) => goCounter.inc()(using state.localUid)
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
      (ew, r) => if r % 2 != 0 then ew.enable(using state.localUid)() else ew.disable()
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
      (ew, r) => if r % 2 != 0 then ew.enable(using state.localUid)() else ew.disable()
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
      (ew, r) => if (r % 2) != 0 then ew.enable(using state.localUid)() else ew.disable()
    )
  }

  @Benchmark
  def baselineBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r) => mvRegister.write(r)(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer =
      DeltaBufferNonRedundant[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r) => mvRegister.write(r)(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferMVRegister(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[MultiVersionRegister[Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      MultiVersionRegister.empty[Int],
      (mvRegister, r) => mvRegister.write(r)(using state.localUid)
    )
  }

  @Benchmark
  def baselineBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer =
      DeltaBufferEverything[ObserveRemoveMap[Int, Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r, r)(using state.localUid)
    )
  }

  @Benchmark
  def nonRedundantBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
      given Lattice[Int] = math.max
      Lattice.derived
    val deltaBuffer =
      DeltaBufferNonRedundant[ObserveRemoveMap[Int, Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r, r)(using state.localUid)
    )
  }

  @Benchmark
  def subsumedBufferORMap(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Bottom[Int]                         = Bottom.provide(0)
    given Lattice[ObserveRemoveMap[Int, Int]] =
      given Lattice[Int] = math.max
      Lattice.derived

    val deltaBuffer = DeltaBufferSubsumed[ObserveRemoveMap[Int, Int]]()

    Eval.modReplica(
      deltaBuffer,
      blackhole,
      state,
      resultCapture,
      ObserveRemoveMap.empty[Int, Int],
      (orMap, r) => orMap.update(r, r)(using state.localUid)
    )
  }

  @Benchmark
  def baselineBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferEverything[NestedKeepRemoveList[Int]]()
    val krList = NestedKeepRemoveList.empty[Int]
    val replica = Replica(state.localUid, krList, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performKRListOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferNonRedundant[NestedKeepRemoveList[Int]]()
    val krList = NestedKeepRemoveList.empty[Int]
    val replica = Replica(state.localUid, krList, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performKRListOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferKRList(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    given Lattice[Int] = math.max
    val deltaBuffer = DeltaBufferSubsumed[NestedKeepRemoveList[Int]]()
    val krList = NestedKeepRemoveList.empty[Int]
    val replica = Replica(state.localUid, krList, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performKRListOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def baselineBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[RecipeBook]()
    val recipeBook = RecipeBook.empty
    val replica = Replica(state.localUid, recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[RecipeBook]()
    val recipeBook = RecipeBook.empty
    val replica = Replica(state.localUid, recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferRecipeBook(blackhole: Blackhole, state: EvalState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[RecipeBook]()
    val recipeBook = RecipeBook.empty
    val replica = Replica(state.localUid, recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

}

object Eval {

  inline def modReplica[A: {Bottom as AB, Lattice}, B <: DeltaBuffer[A, B]](
     deltaBuffer: DeltaBuffer[A, B],
     blackhole: Blackhole,
     state: EvalState,
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

  private def lottery(keys: List[String], random: Int): String = {
    val randomIndex = math.abs(random % keys.size)
    keys(randomIndex)
  }

  def performRecipeBookOperation(recipeBook: RecipeBook, random: Int, replicaID: LocalUid): RecipeBook = {
    def addIngredient(): RecipeBook = {
      val randomRecipeKey = lottery(recipeBook.keys.toList, random)
      recipeBook.addIngredient(randomRecipeKey, Ingredient(random.toString, random.toDouble, random.toString))(using replicaID)
    }

    (random % 10) match {
      case 0 if recipeBook.nonEmpty => {
        val randomKey = lottery(recipeBook.keys.toList, random)
        recipeBook.deleteRecipe(randomKey)
      }
      case 1 if recipeBook.nonEmpty => {
        val randomKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateRecipeTitle(randomKey, random.toString)(using replicaID)
      }
      case 2 if recipeBook.nonEmpty => addIngredient()
      case 3 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        val ingredientsSize = recipeBook.get(randomRecipeKey).get.ingredients.size
        if ingredientsSize > 0 then {
          val randomIngredientIndex = random % ingredientsSize
          recipeBook.updateIngredient(randomRecipeKey, randomIngredientIndex, _ => Ingredient(random.toString, random.toDouble, random.toString))(using replicaID)
        } else addIngredient()
      }
      case 4 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        val ingredientsSize = recipeBook.get(randomRecipeKey).get.ingredients.size
        if ingredientsSize > 0 then {
          val randomIngredientIndex = random % ingredientsSize
          recipeBook.deleteIngredient(randomRecipeKey, randomIngredientIndex)(using replicaID)
        } else addIngredient()
      }
      case 5 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateServings(randomRecipeKey, random)(using replicaID)
      }
      case 6 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateCookingTime(randomRecipeKey, random)(using replicaID)
      }
      case 7 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateDescription(randomRecipeKey, random.toString)(using replicaID)
      }
      case 8 if recipeBook.nonEmpty => {
        val randomRecipeKey = lottery(recipeBook.keys.toList, random)
        recipeBook.updateFavorite(randomRecipeKey, random%2 == 0)(using replicaID)
      }
      case _ => {
        val recipe = Recipe.empty
        recipeBook.addRecipe(random.toString, recipe)(using replicaID)
      }
    }
  }

  def performKRListOperation(krList: NestedKeepRemoveList[Int], random: Int, replicaID: LocalUid): NestedKeepRemoveList[Int] = {
    if krList.size == 0 then return krList.insertAt(0, random)(using replicaID)
    (random % 3) match {
      case 0 => {
        val index = math.abs(random % krList.size)
        krList.remove(index)(using replicaID)
      }
      case 1 => {
        val index = math.abs(random % krList.size)
        krList.update(index, (_) => random)(using replicaID)
      }
      case _ => {
        val index = math.abs(random % krList.size)
        krList.insertAt(index, random)(using replicaID)
      }
    }
  }

  def initializeRecipeBook(localUid: LocalUid, num: Int): RecipeBook = {
    var recipeBook: RecipeBook = RecipeBook.empty

    (0 to num).foreach( i =>
      val ingredients: Iterable[Ingredient] = (0 to i%10).map(j => Ingredient(j.toString, j, j.toString))
      val recipe: Recipe = Recipe(i.toString, ingredients)(using localUid)
      recipeBook = recipeBook `merge` recipeBook.addRecipe(i.toString, recipe)(using localUid)
    )

    recipeBook
  }

}
