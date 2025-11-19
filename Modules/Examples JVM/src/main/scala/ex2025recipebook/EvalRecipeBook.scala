package ex2025recipebook

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Historized, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, GrowOnlyCounter, LastWriterWins, MultiVersionRegister, ObserveRemoveMap}
import rdts.time.Dots

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class EvalRecipeBookState {

  @Param(Array("1", "2", "5", "10", "20", "50", "100"))
  var numOperations: Int   = 0
  val random               = new scala.util.Random(123456789)
  var randomArr: List[Int] = List.empty
  val localUid: LocalUid = LocalUid.gen()

  var recipeBook: RecipeBook = RecipeBook.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    randomArr = List.fill(1000)(random.nextInt())
    recipeBook = Eval.initializeRecipeBook(localUid, numOperations)
  }

}

@Fork(value = 1, warmups = 0)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DeltaBufferRecipeBookBenchmark {

  @Benchmark
  def baselineBufferRecipeBook(blackhole: Blackhole, state: EvalRecipeBookState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferEverything[RecipeBook]()
    val replica = Replica(state.localUid, state.recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def nonRedundantBufferRecipeBook(blackhole: Blackhole, state: EvalRecipeBookState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferNonRedundant[RecipeBook]()
    val replica = Replica(state.localUid, state.recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }

  @Benchmark
  def subsumedBufferRecipeBook(blackhole: Blackhole, state: EvalRecipeBookState, resultCapture: ResultCapture): Unit = {
    val deltaBuffer = DeltaBufferSubsumed[RecipeBook]()
    val replica = Replica(state.localUid, state.recipeBook, deltaBuffer)

    blackhole.consume(state.randomArr.foreach { item =>
      replica.mod(a => Eval.performRecipeBookOperation(a, item, state.localUid))
      resultCapture.recordBufferSize(replica.buffer.getSize)
    })
  }
}
