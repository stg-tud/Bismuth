package benchmarks.b2026merklesearchtree

import org.openjdk.jmh.annotations.*
import rdts.time.CausalTime
import channels.JsoniterCodecs.given
import channels.experiments.MerkleSearchTree

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class MerkleSearchTreeBenchmark {

  @Param(Array("1000000"))
  var size: Int = scala.compiletime.uninitialized

  @Param(Array("4", "16", "64"))
  var branchingFactor: Int = scala.compiletime.uninitialized

  @Param(Array("32"))
  var payloadBytes: Int = scala.compiletime.uninitialized

  private given Ordering[CausalTime] = CausalTime.ordering

  private var baseTree: MerkleSearchTree[CausalTime]                        = scala.compiletime.uninitialized
  private var slightlyOlderTree: MerkleSearchTree[CausalTime]               = scala.compiletime.uninitialized
  private var highTimestampEntry: MerkleSearchTree.Entry[CausalTime]        = scala.compiletime.uninitialized
  private var nextTimestampEntry: MerkleSearchTree.Entry[CausalTime]        = scala.compiletime.uninitialized
  private var updatedHighTimestampEntry: MerkleSearchTree.Entry[CausalTime] = scala.compiletime.uninitialized
  private var removableHash: MerkleSearchTree.Hash                          = scala.compiletime.uninitialized
  private var updatableHash: MerkleSearchTree.Hash                          = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val entries = Vector.tabulate(size)(makeEntry)
    baseTree = MerkleSearchTree.fromEncodedEntries(entries, branchingFactor)

    val slightlyOlderSize = math.max(0, size - math.max(1, size / 64))
    slightlyOlderTree = MerkleSearchTree.fromEncodedEntries(entries.take(slightlyOlderSize), branchingFactor)

    highTimestampEntry = makeEntryAt(size.toLong + 1_000_000_000L, size + 17)
    nextTimestampEntry = makeEntryAt(size.toLong, size + 23)

    val middleIndex = math.max(0, size / 2)
    removableHash = entries(middleIndex).hash
    updatableHash = entries(math.max(0, size / 3)).hash
    updatedHighTimestampEntry = makeEntryAt(size.toLong + 2_000_000_000L, size + 42)
  }

  @Benchmark
  def insertEncodedHighCausalTime(): MerkleSearchTree[CausalTime] =
    baseTree.insertEncoded(highTimestampEntry)

  @Benchmark
  def insertEncodedNextCausalTime(): MerkleSearchTree[CausalTime] =
    baseTree.insertEncoded(nextTimestampEntry)

  @Benchmark
  def insertHighCausalTimeIntoEmpty(): MerkleSearchTree[CausalTime] =
    MerkleSearchTree.empty[CausalTime](branchingFactor).insertEncoded(highTimestampEntry)

  @Benchmark
  def removeExisting(): MerkleSearchTree[CausalTime] =
    baseTree.remove(removableHash)

  @Benchmark
  def updateToHighCausalTime(): MerkleSearchTree[CausalTime] =
    baseTree.updateEncoded(updatableHash, updatedHighTimestampEntry)

  @Benchmark
  def compareAgainstSlightlyOlderReplica(): Vector[MerkleSearchTree.Entry[CausalTime]] =
    baseTree.missingFrom(slightlyOlderTree)

  private def makeEntry(index: Int): MerkleSearchTree.Entry[CausalTime] =
    makeEntryAt(index.toLong, index)

  private def makeEntryAt(time: Long, salt: Int): MerkleSearchTree.Entry[CausalTime] =
    MerkleSearchTree.Entry(
      timestamp = CausalTime(time, 0, time),
      encoded = payload(salt),
      hash = MerkleSearchTree.entryHash(CausalTime(time, 0, time), payload(salt)),
    )

  private def payload(salt: Int): Array[Byte] = {
    val arr = new Array[Byte](payloadBytes)
    var i   = 0
    while i < arr.length do
        arr(i) = ((salt * 31 + i * 17) & 0xff).toByte
        i += 1
    arr
  }
}
