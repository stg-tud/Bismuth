package ex2024textRDT

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.openjdk.jmh.annotations.*
import text_rdt.*
import text_rdt.fugue.complex.{*, given}
import text_rdt.fugue.complexavl.{*, given}
import text_rdt.fugue.simple.{*, given}
import text_rdt.fugue.simpleavl.{*, given}
import text_rdt.fugue.{*, given}

import java.lang.management.ManagementFactory
import javax.management.ObjectName
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.io.Source

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

  private given stringCodec: JsonValueCodec[String] = JsonCodecMaker.make

  private def readOperation(in: JsonReader): FixtureOperation = {
    if in.nextToken() != '['.toByte then {
      in.decodeError("expected operation array")
    }
    val index = in.readInt()
    if in.nextToken() != ','.toByte then {
      in.decodeError("expected ',' after operation index")
    }
    val length = in.readInt()
    in.nextToken() match {
      case ',' =>
        val string = in.readString("")
        assert(length == 0)
        assert(string.length == 1)
        if in.nextToken() != ']'.toByte then {
          in.decodeError("expected end of operation array")
        }
        FixtureOperation.Insert(index, string.charAt(0))
      case ']' =>
        assert(length == 1)
        FixtureOperation.Delete(index)
      case _ =>
        in.decodeError("expected ',' or ']' after operation length")
    }
  }

  private given fixtureOperationsCodec: JsonValueCodec[Seq[FixtureOperation]] = {
    new JsonValueCodec[Seq[FixtureOperation]] {

      override def decodeValue(
          in: JsonReader,
          default: Seq[FixtureOperation]
      ): Seq[FixtureOperation] = {
        val operations = ArrayBuffer.empty[FixtureOperation]
        if in.nextToken() != '['.toByte then {
          in.decodeError("expected operations array")
        }
        var token = in.nextToken()
        if token != ']'.toByte then {
          in.rollbackToken()
          var done = false
          while !done do {
            operations += readOperation(in)
            token = in.nextToken()
            if token == ','.toByte then {
              ()
            } else if token == ']'.toByte then {
              done = true
            } else {
              in.decodeError("expected ',' or ']'")
            }
          }
        }
        operations.toSeq
      }

      override def encodeValue(
          x: Seq[FixtureOperation],
          out: JsonWriter
      ): Unit = {
        out.writeArrayStart()
        x.foreach {
          case FixtureOperation.Insert(position, character) =>
            out.writeArrayStart()
            out.writeVal(position)
            out.writeVal(0)
            out.writeVal(character.toString)
            out.writeArrayEnd()
          case FixtureOperation.Delete(position) =>
            out.writeArrayStart()
            out.writeVal(position)
            out.writeVal(1)
            out.writeArrayEnd()
        }
        out.writeArrayEnd()
      }

      override def nullValue: Seq[FixtureOperation] =
        null.asInstanceOf[Seq[FixtureOperation]]
    }
  }

  val finalString: String =
    readFromString[String](
      Source
        .fromInputStream(
          getClass.getResourceAsStream("/ex2024textRDT/data/final.json").nn
        )
        .mkString
    )

  val operations: ArrayBuffer[FixtureOperation] =
    ArrayBuffer.from(
      readFromString[Seq[FixtureOperation]](
        Source
          .fromInputStream(
            getClass.getResourceAsStream("/ex2024textRDT/data/edits.json").nn
          )
          .mkString
      )
    )
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

  import Counters.*

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("simple", "complex", "simpleavl", "complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10000", "20000", "259778"))
  @annotation.nowarn("msg=unset private variable")
  private var count: Int = uninitialized

  @Benchmark
  def local(counters: Counters): Option[Replica[?]] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = Some(localMethod(count))
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  private def localMethod(
      size: Int
  ): Replica[
    text_rdt.fugue.simple.SimpleFugueFactory.simpleFugueFactory.type |
    text_rdt.fugue.complex.ComplexFugueFactory.complexFugueFactory.type |
    text_rdt.fugue.simpleavl.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
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
        case "simple"    => SimpleFugueFactory.simpleFugueFactory
        case "complex"   => ComplexFugueFactory.complexFugueFactory
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
    ](replicaStateA, StringEditory())
    MyRealWorldBenchmark.operations.view
      .slice(0, size)
      .foreach {
        case FixtureOperation.Insert(position, character) =>
          replicaA.insert(position, character)
        case FixtureOperation.Delete(position) =>
          replicaA.delete(position)
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

  import Counters.*

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10", "50"))
  var count: Int = uninitialized

  @Benchmark
  def local(counters: Counters): Option[Replica[?]] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = Some(localMethod())
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  def localMethod(
  ): Replica[
    text_rdt.fugue.simple.SimpleFugueFactory.simpleFugueFactory.type |
    text_rdt.fugue.complex.ComplexFugueFactory.complexFugueFactory.type |
    text_rdt.fugue.simpleavl.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
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
        case "simple"    => SimpleFugueFactory.simpleFugueFactory
        case "complex"   => ComplexFugueFactory.complexFugueFactory
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
    ](replicaStateA, StringEditory())
    var index          = 0
    val operations     = MyRealWorldBenchmark.operations
    val operationsSize = operations.size
    while index < count do {
      var i = 0
      while i < operationsSize do {
        operations(i) match {
          case FixtureOperation.Insert(position, character) =>
            replicaA.insert(
              index * 104852 + position,
              character
            )
          case FixtureOperation.Delete(position) =>
            replicaA.delete(
              index * 104852 + position
            )
        }
        i += 1
      }
      index += 1
    }
    if shouldMeasureMemory then {
      assert(replicaA.text() == MyRealWorldBenchmark.finalString * count)
    }
    replicaA
  }
}

@State(Scope.Benchmark)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled", "-Xmx8G"))
class MyRealWorldBenchmarkLargeRemote {

  import Counters.*

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(Array("10", "50"))
  var count: Int = uninitialized

  var replicaWithOperationsApplied: Replica[
    text_rdt.fugue.simple.SimpleFugueFactory.simpleFugueFactory.type |
    text_rdt.fugue.complex.ComplexFugueFactory.complexFugueFactory.type |
    text_rdt.fugue.simpleavl.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = uninitialized

  @Setup(Level.Trial)
  def setupReplicaWithOperationsApplied(): Unit =
    replicaWithOperationsApplied = localMethod()

  private def localMethod(): Replica[
    text_rdt.fugue.simple.SimpleFugueFactory.simpleFugueFactory.type |
    text_rdt.fugue.complex.ComplexFugueFactory.complexFugueFactory.type |
    text_rdt.fugue.simpleavl.SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
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
        case "simple"    => SimpleFugueFactory.simpleFugueFactory
        case "complex"   => ComplexFugueFactory.complexFugueFactory
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
    ](replicaStateA, StringEditory())
    (0 until count).foreach(index =>
      MyRealWorldBenchmark.operations.view
        .foreach {
          case FixtureOperation.Insert(position, character) =>
            replicaA.insert(
              index * 104852 + position,
              character
            )
          case FixtureOperation.Delete(position) =>
            replicaA.delete(
              index * 104852 + position
            )
        }
    )
    if shouldMeasureMemory then {
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
        case "simple"    => SimpleFugueFactory.simpleFugueFactory
        case "complex"   => ComplexFugueFactory.complexFugueFactory
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
    ](replicaStateA, StringEditory())
    replicaA.applyRemoteUnchecked(
      replicaWithOperationsApplied.state.outgoing.asInstanceOf[Iterable[replicaA.state.factoryContext.MSG]]
    )
    if shouldMeasureMemory then {
      assert(replicaA.text() == MyRealWorldBenchmark.finalString * count)
    }
    Some(replicaA)
  }

  @Benchmark
  def remote(counters: Counters): Option[Replica[?]] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = remoteMethod()
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }
}

@State(Scope.Thread)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled"))
class MyTerribleBenchmarkSequentialInserts {

  import Counters.*

  @Param(Array("false"))
  var shouldMeasureMemory: Boolean = uninitialized

  @Param(Array("simple", "complex", "simpleavl", "complexavl"))
  var factoryConstructor: String = uninitialized

  @Param(
    Array(
      "10000"
    )
  )
  @annotation.nowarn("msg=unset private variable")
  private var count: Int = uninitialized

  @Benchmark
  def sequentialInserts(counters: Counters): Replica[?] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = sequentialInserts(count)
    if shouldMeasureMemory then {
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
          case "simple"    => SimpleFugueFactory.simpleFugueFactory
          case "complex"   => ComplexFugueFactory.complexFugueFactory
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
    ](replicaStateA, StringEditory())
    for i <- 0 until size do
        replicaA.insert(i, 'a')
    if shouldMeasureMemory then {
      assert(replicaStateA.text().equals("a" * size))
    }
    replicaA
  }
}

@State(Scope.Thread)
@Fork(jvmArgsAppend = Array("-Dtextrdt.assertions=disabled"))
class MyTerribleBenchmarkEvilInsert {

  import Counters.*

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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    for i <- 0 until until do
        replicaToSyncInto.insert(0, 'p')
    replicaToSyncInto
  }

  @Benchmark
  def evilInsert1SimpleAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = evilInsert1("simpleavl")
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert1ComplexAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type |
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
      counters.memory = -measureMemory()
    }
    val result = evilInsert1("complexavl")
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert2SimpleAVL(counters: Counters): Replica[
    SimpleAVLFugueFactory.simpleAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    for i <- 0 until count do
        replicaToSyncInto.insert(i, 'p')
    replicaToSyncInto.applyRemoteUnchecked(
      (1 to until).map(i =>
        Message.Insert(
          SimpleID("b", i),
          '.',
          SimpleID("a", i),
          Side.Right
        )
      )
    )

    val result = replicaToSyncInto
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilInsert2ComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    for i <- 0 until count do
        replicaToSyncInto.insert(
          i,
          'p'
        )
    replicaToSyncInto.applyRemoteUnchecked(
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

    val result = replicaToSyncInto
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilSplitComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    for i <- 0 until count do
        replicaToSyncInto.insert(
          i,
          'p'
        )
    for i <- 0 until until / 2 do
        replicaToSyncInto.delete(
          i + 1
        )
    val result = replicaToSyncInto
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilSplitManyRightChildrenComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    for i <- 0 until count do
        replicaToSyncInto.insert(
          i,
          'p'
        )
    replicaToSyncInto.applyRemoteUnchecked(
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
    for i <- 1 until until * 2 - 2 by 2 do
        replicaToSyncInto.insert(
          i,
          's'
        )
    val result = replicaToSyncInto
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }

  @Benchmark
  def evilChildrenComplexAVL(counters: Counters): Replica[
    ComplexAVLFugueFactory.complexAVLFugueFactory.type
  ] = {
    if shouldMeasureMemory then {
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
    val replicaToSyncInto = Replica(replicaStateToSyncInto, StringEditory())
    replicaToSyncInto.applyRemoteUnchecked(
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

    val result = replicaToSyncInto
    if shouldMeasureMemory then {
      counters.memory += measureMemory()
    }
    result
  }
}
