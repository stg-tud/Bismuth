package benchmarks.b2026collections

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.collection.concurrent.TrieMap
import scala.util.Random

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 5, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms1g", "-Xmx1g"))
class ThreadsafeMapOverheadBenchmark {

  // Number of entries to put into each map
  @Param(Array("10000", "100000"))
  var size: Int = scala.compiletime.uninitialized

  // Shared random input data, identical for every implementation
  var keys: Array[String] = scala.compiletime.uninitialized
  var values: Array[Long] = scala.compiletime.uninitialized

  // Pre-filled maps used for the "get" benchmarks
  var filledTrieMap: TrieMap[String, Long]                     = TrieMap.empty[String, Long]
  var filledAtomicRef: AtomicReference[Map[String, Long]]      = AtomicReference(Map.empty)
  var filledConcurrentHashMap: ConcurrentHashMap[String, Long] = new ConcurrentHashMap[String, Long](size)

  @Setup(Level.Trial)
  def setup(): Unit = {
    val random = new Random(42L) // fixed seed for reproducibility

    keys = Array.fill(size)(random.nextLong().toString)
    values = Array.fill(size)(random.nextLong())

    var immutableMap = Map.empty[String, Long]
    var i            = 0
    while i < size do {
      filledTrieMap.put(keys(i), values(i))
      immutableMap = immutableMap.updated(keys(i), values(i))
      filledConcurrentHashMap.put(keys(i), values(i))
      i += 1
    }

    filledAtomicRef = new AtomicReference[Map[String, Long]](immutableMap)
  }

  // ---------- PUT ----------

  @Benchmark
  def putTrieMap(): TrieMap[String, Long] = {
    val map = TrieMap.empty[String, Long]
    var i   = 0
    while i < size do {
      map.put(keys(i), values(i))
      i += 1
    }
    map
  }

  @Benchmark
  def putAtomicRefImmutableMap(): AtomicReference[Map[String, Long]] = {
    val ref = new AtomicReference[Map[String, Long]](Map.empty[String, Long])
    var i   = 0
    while i < size do {
      val k       = keys(i)
      val v       = values(i)
      var updated = false
      while !updated do {
        val current = ref.get()
        val next    = current.updated(k, v)
        updated = ref.compareAndSet(current, next)
      }
      i += 1
    }
    ref
  }

  @Benchmark
  def putConcurrentHashMap(): ConcurrentHashMap[String, Long] = {
    val map = new ConcurrentHashMap[String, Long](size)
    var i   = 0
    while i < size do {
      map.put(keys(i), values(i))
      i += 1
    }
    map
  }

  // ---------- GET ----------

  @Benchmark
  def getTrieMap(bh: Blackhole): Unit = {
    var i = 0
    while i < size do {
      bh.consume(filledTrieMap.get(keys(i)))
      i += 1
    }
  }

  @Benchmark
  def getAtomicRefImmutableMap(bh: Blackhole): Unit = {
    val snapshot = filledAtomicRef.get()
    var i        = 0
    while i < size do {
      bh.consume(snapshot.get(keys(i)))
      i += 1
    }
  }

  @Benchmark
  def getConcurrentHashMap(bh: Blackhole): Unit = {
    var i = 0
    while i < size do {
      bh.consume(filledConcurrentHashMap.get(keys(i)))
      i += 1
    }
  }
}
