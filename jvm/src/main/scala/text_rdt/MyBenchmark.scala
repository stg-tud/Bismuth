package text_rdt

import org.openjdk.jmh.annotations.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.io.Source
import java.lang.management.ManagementFactory
import javax.management.ObjectName

enum FixtureOperation {
  case Insert(position: Int, character: Char)
  case Delete(position: Int)
}

def measureMemory(): Long = {
  val histogram = ManagementFactory
    .getPlatformMBeanServer()
    .nn
    .invoke(
      new ObjectName("com.sun.management:type=DiagnosticCommand"),
      "gcClassHistogram",
      Array[Object | Null](null),
      Array("[Ljava.lang.String;")
    )
    .asInstanceOf[String];
  histogram
    .substring(histogram.lastIndexOf("\n", histogram.size - 2))
    .nn
    .trim()
    .nn
    .split("\\s+")
    .nn(2)
    .nn
    .toLong
}

object MyRealWorldBenchmark {

  val finalString: String = {
    ujson
      .read(
        Source
          .fromInputStream(
            getClass.getResourceAsStream("/data/final.json").nn
          )
          .mkString
      )
      .str
  }

  val operations: ArrayBuffer[FixtureOperation] = {
    val json =
      ujson
        .read(
          Source
            .fromInputStream(
              getClass.getResourceAsStream("/data/edits.json").nn
            )
            .mkString
        )
        .arr
    json.map(item =>
      item.arr.toArray match {
        case Array(first, second, third) =>
          val index = first.num.toInt
          val length = second.num.toInt
          assert(length == 0)
          val string = third.str
          assert(string.length() == 1)
          val char = string.charAt(0)
          FixtureOperation.Insert(index, char)
        case Array(first, second) =>
          val index = first.num.toInt
          val length = second.num.toInt
          assert(length == 1)
          FixtureOperation.Delete(index)
        case default =>
          assert(false)
      }
    )
  }
}

object Counters {

  @State(Scope.Thread)
  @AuxCounters(AuxCounters.Type.EVENTS)
  class Counters {
    var memory: Long = uninitialized
  }
}

@State(Scope.Benchmark)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled"))
class MyRealWorldBenchmarkLocal {

  import Counters._

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("simple", "complex", "simpleavl", "complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10000", "20000", "259778"))
  private var count: Int = uninitialized

  @Benchmark
  def local(counters: Counters): Option[Replica[?]] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = Some(localMethod(count))
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  private def localMethod(
      size: Int
  ): Replica[
    text_rdt.SimpleFugueFactory.simpleFugueFactory.type |
      text_rdt.ComplexFugueFactory.complexFugueFactory.type |
      text_rdt.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    val replicaStateA = ReplicaState[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](
      "A"
    )(using
      factoryConstructor match {
        case "simple"  => SimpleFugueFactory.simpleFugueFactory
        case "complex" => ComplexFugueFactory.complexFugueFactory
        case "simpleavl" =>
          SimpleAVLFugueFactory.simpleAVLFugueFactory
        case "complexavl" =>
          ComplexAVLFugueFactory.complexAVLFugueFactory
      }
    )
    val replicaA = Replica[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](replicaStateA, NoopEditory())
    MyRealWorldBenchmark.operations.view
      .slice(0, size)
      .foreach {
        case FixtureOperation.Insert(position, character) =>
          replicaStateA.insert(position, character)
        case FixtureOperation.Delete(position) =>
          replicaStateA.delete(position)
      }
    replicaA
  }
}

@State(Scope.Benchmark)
@Fork(jvmArgsAppend =
  Array(
    "-Dtextrdt.assertions=disabled",
    "-Xmx8G"
  )
)
class MyRealWorldBenchmarkLargeLocal {

  import Counters._

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10", "50"))
  var count: Int = uninitialized

  @Benchmark
  def local(counters: Counters): Option[Replica[?]] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = Some(localMethod())
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  def localMethod(
  ): Replica[
    text_rdt.SimpleFugueFactory.simpleFugueFactory.type |
      text_rdt.ComplexFugueFactory.complexFugueFactory.type |
      text_rdt.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    val replicaStateA = ReplicaState[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](
      "A"
    )(using
      factoryConstructor match {
        case "simple"  => SimpleFugueFactory.simpleFugueFactory
        case "complex" => ComplexFugueFactory.complexFugueFactory
        case "simpleavl" =>
          SimpleAVLFugueFactory.simpleAVLFugueFactory
        case "complexavl" =>
          ComplexAVLFugueFactory.complexAVLFugueFactory
      }
    )
    val replicaA = Replica[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](replicaStateA, NoopEditory())
    var index = 0
    val operations = MyRealWorldBenchmark.operations
    val operationsSize = operations.size
    while (index < count) {
      var i = 0
      while (i < operationsSize) {
        operations(i) match {
          case FixtureOperation.Insert(position, character) =>
            replicaStateA.insert(
              index * 104852 + position,
              character
            )
          case FixtureOperation.Delete(position) =>
            replicaStateA.delete(
              index * 104852 + position
            )
        }
        i += 1
      }
      index += 1
    }
    if (shouldMeasureMemory) {
      assert(replicaA.text() == MyRealWorldBenchmark.finalString * count)
    }
    replicaA
  }
}

@State(Scope.Benchmark)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled", "-Xmx8G"))
class MyRealWorldBenchmarkLargeRemote {

  import Counters._

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10", "50"))
  var count: Int = uninitialized

  var replicaWithOperationsApplied: Replica[
    text_rdt.SimpleFugueFactory.simpleFugueFactory.type |
      text_rdt.ComplexFugueFactory.complexFugueFactory.type |
      text_rdt.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = uninitialized

  @Setup(Level.Trial)
  def setupReplicaWithOperationsApplied(): Unit = {
    replicaWithOperationsApplied = localMethod()
  }

  private def localMethod(): Replica[
    text_rdt.SimpleFugueFactory.simpleFugueFactory.type |
      text_rdt.ComplexFugueFactory.complexFugueFactory.type |
      text_rdt.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {

    val replicaStateA = ReplicaState[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](
      "A"
    )(using
      factoryConstructor match {
        case "simple"  => SimpleFugueFactory.simpleFugueFactory
        case "complex" => ComplexFugueFactory.complexFugueFactory
        case "simpleavl" =>
          SimpleAVLFugueFactory.simpleAVLFugueFactory
        case "complexavl" =>
          ComplexAVLFugueFactory.complexAVLFugueFactory
      }
    )
    val replicaA = Replica[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](replicaStateA, NoopEditory())
    (0 until count).foreach(index => {
      MyRealWorldBenchmark.operations.view
        .foreach {
          case FixtureOperation.Insert(position, character) =>
            replicaStateA.insert(
              index * 104852 + position,
              character
            )
          case FixtureOperation.Delete(position) =>
            replicaStateA.delete(
              index * 104852 + position
            )
        }
    })
    if (shouldMeasureMemory) {
      assert(replicaA.text() == MyRealWorldBenchmark.finalString * count)
    }
    replicaA
  }

  def remoteMethod(): Option[Replica[?]] = {
    val replicaStateA = ReplicaState[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](
      "B"
    )(using
      factoryConstructor match {
        case "simple"  => SimpleFugueFactory.simpleFugueFactory
        case "complex" => ComplexFugueFactory.complexFugueFactory
        case "simpleavl" =>
          SimpleAVLFugueFactory.simpleAVLFugueFactory
        case "complexavl" =>
          ComplexAVLFugueFactory.complexAVLFugueFactory
      }
    )
    val replicaA = Replica[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](replicaStateA, NoopEditory())
    replicaA.syncFrom(
      replicaWithOperationsApplied.state
    )
    if (shouldMeasureMemory) {
      assert(replicaA.text() == MyRealWorldBenchmark.finalString * count)
    }
    Some(replicaA)
  }

  @Benchmark
  def remote(counters: Counters): Option[Replica[?]] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = remoteMethod()
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }
}

@State(Scope.Thread)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled"))
class MyTerribleBenchmarkSequentialInserts {

  import Counters._

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("simple", "complex", "simpleavl", "complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(
    Array(
      "10000"
    )
  )
  private var count: Int = uninitialized

  @Benchmark
  def sequentialInserts(counters: Counters): Replica[?] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = sequentialInserts(count)
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  private def sequentialInserts(
      size: Int
  ): Replica[?] = {
    val replicaStateA =
      ReplicaState[
        SimpleFugueFactory.simpleFugueFactory.type |
          ComplexFugueFactory.complexFugueFactory.type |
          SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
          ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "A"
      )(using
        factoryConstructor match {
          case "simple"  => SimpleFugueFactory.simpleFugueFactory
          case "complex" => ComplexFugueFactory.complexFugueFactory
          case "simpleavl" =>
            SimpleAVLFugueFactory.simpleAVLFugueFactory
          case "complexavl" =>
            ComplexAVLFugueFactory.complexAVLFugueFactory
        }
      )
    val replicaA = Replica[
      SimpleFugueFactory.simpleFugueFactory.type |
        ComplexFugueFactory.complexFugueFactory.type |
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
    ](replicaStateA, NoopEditory())
    for (i <- 0 until size) {
      replicaA.state.insert(i, 'a')
    }
    if (shouldMeasureMemory) {
      assert(replicaStateA.text().equals("a" * size))
    }
    replicaA
  }
}

@State(Scope.Thread)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled"))
class MyTerribleBenchmarkEvilInsert {

  import Counters._

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  var until: Int = uninitialized

  @Param(Array("100000"))
  var count: Int = uninitialized

  @Setup
  def checkAssertionsDisabled(): Unit = {
    until = count;
    assert(
      !Helper.ENABLE,
      "Disable assertions (Helper.ENABLE) for benchmarking"
    )
  }

  private def evilInsert1(factoryConstructor: String): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    val replicaStateToSyncInto =
      ReplicaState[
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
          ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "a"
      )(using
        factoryConstructor match {
          case "simpleavl" =>
            SimpleAVLFugueFactory.simpleAVLFugueFactory
          case "complexavl" =>
            ComplexAVLFugueFactory.complexAVLFugueFactory
        }
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    for (i <- 0 until until) {
      replicaToSyncInto.state.insert(0, 'p')
    }
    replicaToSyncInto
  }

  @Benchmark
  def evilInsert1SimpleAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = evilInsert1("simpleavl")
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert1ComplexAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
      ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val result = evilInsert1("complexavl")
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert2SimpleAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val replicaStateToSyncInto =
      ReplicaState[
        SimpleAVLFugueFactory.simpleAVLFugueFactory.type
      ](
        "a"
      )(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    for (i <- 0 until count) {
      replicaToSyncInto.state.insert(i, 'p')
    }
    replicaToSyncInto.deliveringRemote(
      (
        mutable.HashMap.empty,
        mutable.ArrayBuffer.from(
          (1 to until).map(i =>
            Message.Insert(
              SimpleID("b", i),
              '.',
              SimpleID("a", i),
              Side.Right
            )
          )
        )
      )
    )

    val result = replicaToSyncInto
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert2ComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val replicaStateToSyncInto =
      ReplicaState[
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "a"
      )(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    for (i <- 0 until count) {
      replicaToSyncInto.state.insert(
        i,
        'p'
      )
    }
    replicaToSyncInto.deliveringRemote(
      (
        mutable.HashMap.empty,
        mutable.ArrayBuffer.from(
          (0 until until).map(i =>
            ComplexAVLMessage.Insert(
              "b",
              i,
              0,
              StringBuilder("c"),
              "a",
              1,
              i,
              Side.Right
            )
          )
        )
      )
    )

    val result = replicaToSyncInto
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilSplitComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val replicaStateToSyncInto =
      ReplicaState[
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "a"
      )(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    for (i <- 0 until count) {
      replicaToSyncInto.state.insert(
        i,
        'p'
      )
    }
    for (i <- 0 until until / 2) {
      replicaToSyncInto.state.delete(
        i + 1
      )
    }
    val result = replicaToSyncInto
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilSplitManyRightChildrenComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val replicaStateToSyncInto =
      ReplicaState[
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "a"
      )(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    for (i <- 0 until count) {
      replicaToSyncInto.state.insert(
        i,
        'p'
      )
    }
    replicaToSyncInto.deliveringRemote(
      (
        mutable.HashMap.empty,
        mutable.ArrayBuffer.from(
          (0 until count).map(i =>
            ComplexAVLMessage.Insert(
              s"b$i",
              0,
              0,
              StringBuilder("c"),
              "a",
              1,
              count - 1,
              Side.Right
            )
          )
        )
      )
    )
    for (i <- 1 until until * 2 - 2 by 2) {
      replicaToSyncInto.state.insert(
        i,
        's'
      )
    }
    val result = replicaToSyncInto
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilChildrenComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if (shouldMeasureMemory) {
      counters.memory = -measureMemory()
    }
    val replicaStateToSyncInto =
      ReplicaState[
        ComplexAVLFugueFactory.complexAVLFugueFactory.type
      ](
        "a"
      )(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaToSyncInto = Replica(replicaStateToSyncInto, NoopEditory())
    replicaToSyncInto.deliveringRemote(
      (
        mutable.HashMap.empty,
        mutable.ArrayBuffer.from(
          (0 until until).map(i =>
            ComplexAVLMessage.Insert(
              s"b$i",
              0,
              0,
              StringBuilder("c"),
              null,
              0,
              0,
              Side.Right
            )
          )
        )
      )
    )

    val result = replicaToSyncInto
    if (shouldMeasureMemory) {
      counters.memory += measureMemory()
    }
    result
  }
}
