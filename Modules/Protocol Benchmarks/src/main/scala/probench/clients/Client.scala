package probench.clients

import probench.benchmark.{BenchmarkData, CSVWriter}
import probench.data.KVOperation
import rdts.base.Uid

import java.nio.file.Path
import scala.collection.mutable

enum BenchmarkMode {
  case Fixed
  case Timed
}

enum BenchmarkOpType {
  case Read
  case Write
  case Mixed
}

trait Client(name: Uid, logTimings: Boolean) {

  var printResults                                     = true
  var doBenchmark: Boolean                             = false
  val benchmarkData: mutable.ListBuffer[BenchmarkData] = mutable.ListBuffer.empty

  def read(key: String): Unit                 = handleOp(KVOperation.Read(key))
  def write(key: String, value: String): Unit = handleOp(KVOperation.Write(key, value))

  def log(f: => String): Unit = if logTimings then println(f)

  def multiget(key: String, times: Int, offset: Int = 0): Unit = {
    val start = System.nanoTime()
    for i <- offset to (times + offset) do read(key.replace("%n", i.toString))
    log(s"Did $times get queries in ${(System.nanoTime() - start) / 1_000_000}ms")
    log(s"Did ${times.toDouble / ((System.nanoTime() - start) / 1_000_000_000d)} ops/s")
  }

  def randomMultiGet(key: String, times: Int, min: Int, max: Int): Unit = {
    val start = System.nanoTime()
    for _ <- 0 to times do {
      val index = Math.round(min + Math.random() * (max - min)).toInt
      read(key.replace("%n", index.toString))
    }
    log(s"Did $times get queries in ${(System.nanoTime() - start) / 1_000_000}ms")
    log(s"Did ${times.toDouble / ((System.nanoTime() - start) / 1_000_000_000d)} ops/s")
  }

  def multiput(key: String, value: String, times: Int, offset: Int = 0): Unit = {
    val start = System.nanoTime()
    for i <- offset to (times + offset) do write(key.replace("%n", i.toString), value.replace("%n", i.toString))
    log(s"Did $times put queries in ${(System.nanoTime() - start) / 1_000_000}ms")
    log(s"Did ${times.toDouble / ((System.nanoTime() - start) / 1_000_000_000d)} ops/s")
  }

  def randomMultiPut(key: String, value: String, times: Int, min: Int, max: Int): Unit = {
    val start = System.nanoTime()
    for _ <- 0 to times do {
      val index = Math.round(min + Math.random() * (max - min)).toInt
      write(key.replace("%n", index.toString), value.replace("%n", index.toString))
    }
    log(s"Did $times put queries in ${(System.nanoTime() - start) / 1_000_000}ms")
    log(s"Did ${times.toDouble / ((System.nanoTime() - start) / 1_000_000_000d)} ops/s")
  }

  def mixed(times: Int, min: Int, max: Int): Unit = {
    val start = System.nanoTime()
    for i <- 1 to times do {
      val num = Math.round(Math.random() * (max - min) + min).toInt
      if Math.random() > 0.5 then
         read(f"key$num")
      else
         write(f"key$num", f"value$num")
    }
    log(s"Did $times mixed queries in ${(System.nanoTime() - start) / 1_000_000}ms")
    log(s"Did ${times.toDouble / ((System.nanoTime() - start) / 1_000_000_000d)} ops/s")
  }

  def benchmarkFixed(mode: BenchmarkOpType, warmup: Int, measurement: Int, min: Int, max: Int): Unit = {
    println("Initializing")

    mode match
       case BenchmarkOpType.Read | BenchmarkOpType.Mixed => multiput("key%n", "value%n", max - min, min)
       case BenchmarkOpType.Write                        =>

    println("Warmup")

    val warmupStart = System.currentTimeMillis()

    mode match
       case BenchmarkOpType.Read  => randomMultiGet("key%n", warmup, min, max)
       case BenchmarkOpType.Write => randomMultiPut("key%n", "value%n", warmup, min, max)
       case BenchmarkOpType.Mixed => mixed(warmup, min, max)

    println("Measurement")

    doBenchmark = true

    val start = System.currentTimeMillis()

    mode match
       case BenchmarkOpType.Read  => randomMultiGet("key%n", measurement, min, max)
       case BenchmarkOpType.Write => randomMultiPut("key%n", "value%n", measurement, min, max)
       case BenchmarkOpType.Mixed => mixed(measurement, min, max)

    val duration = System.currentTimeMillis() - start

    println(s"Did $measurement queries in ${duration}ms")
    println(s"Did ${measurement / (duration / 1000)} op/s")

    saveBenchmark(name)
  }

  def benchmarkTimed(warmup: Int, measurement: Int, mode: BenchmarkOpType, min: Int, max: Int, blockSize: Int): Unit = {
    printResults = false

    println("Initializing")

    mode match
       case BenchmarkOpType.Read | BenchmarkOpType.Mixed => multiput("key%n", "value%n", max - min, min)
       case BenchmarkOpType.Write                        =>

    println("Warmup")

    val warmupStart = System.currentTimeMillis()

    while (System.currentTimeMillis() - warmupStart) < warmup * 1000 do {
      mode match
         case BenchmarkOpType.Read  => randomMultiGet("key%n", blockSize, min, max)
         case BenchmarkOpType.Write => randomMultiPut("key%n", "value%n", blockSize, min, max)
         case BenchmarkOpType.Mixed => mixed(blockSize, min, max)
    }

    println("Measurement")

    val measurementStart = System.currentTimeMillis()
    doBenchmark = true

    var queries = 0

    while (System.currentTimeMillis() - measurementStart) < measurement * 1000 do {
      mode match
         case BenchmarkOpType.Read  => randomMultiGet("key%n", blockSize, min, max)
         case BenchmarkOpType.Write => randomMultiPut("key%n", "value%n", blockSize, min, max)
         case BenchmarkOpType.Mixed => mixed(blockSize, min, max)
      queries += blockSize
    }

    val duration = System.currentTimeMillis() - measurementStart

    println(s"\nDid $queries queries in ${duration}ms")
    println(s"Did ${queries / (duration / 1000)} op/s")

    saveBenchmark(name)
  }

  def handleOp(op: KVOperation[String, String]): Unit = {
    val start = if doBenchmark then System.nanoTime() else 0

    handleOpImpl(op)

    if doBenchmark then {
      val end      = System.nanoTime()
      val opString = op match
         case KVOperation.Read(_)     => "get"
         case KVOperation.Write(_, _) => "put"
      val args = op match
         case KVOperation.Read(key)         => key
         case KVOperation.Write(key, value) => s"$key $value"
      benchmarkData.append(BenchmarkData(
        name.delegate,
        opString,
        args,
        start / 1000,
        end / 1000,
        (end - start).toDouble / 1000,
        "µs"
      ))
    }
  }

  def saveBenchmark(name: Uid): Unit = {
    val env           = System.getenv()
    val runId         = env.getOrDefault("RUN_ID", Uid.gen().delegate)
    val system        = env.getOrDefault("SYSTEM_ID", "pb")
    val benchmarkPath = Path.of(env.getOrDefault("BENCH_RESULTS_DIR", "bench-results")).resolve(system).resolve(runId)
    println(s"Saving Benchmark Data to \n\t$benchmarkPath")
    val writer = new CSVWriter(";", benchmarkPath, s"${name.delegate}-$runId", BenchmarkData.header)
    benchmarkData.foreach { row =>
      writer.writeRow(
        s"${row.name}",
        row.op,
        row.args,
        row.sendTime.toString,
        row.receiveTime.toString,
        row.latency.toString,
        row.unit
      )
    }
    writer.close()
  }

  def handleOpImpl(op: KVOperation[String, String]): Unit

  def onResultValue(result: String): Unit =
    if printResults then println(result)

}
