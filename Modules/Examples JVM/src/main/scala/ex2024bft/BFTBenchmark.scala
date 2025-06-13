package ex2024bft

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Level, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Warmup}
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, ReplicatedList}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class BenchmarkState {
  @Param(Array("1", "2", "5", "10", "25", "50", "100", "250", "500", "1000", "2500", "5000", "10000"))
  var size = 0

  var gocList: List[GrowOnlyCounter]       = null
  var gocLattice: Lattice[GrowOnlyCounter] = null

  var bftGOCList: List[BFT[GrowOnlyCounter]]       = null
  var bftGOCLattice: Lattice[BFT[GrowOnlyCounter]] = null

  var repLL: List[ReplicatedList[Int]]           = null
  var repLLLattice: Lattice[ReplicatedList[Int]] = null

  var bftRepLL: List[BFT[ReplicatedList[Int]]]           = null
  var bftRepLLLattice: Lattice[BFT[ReplicatedList[Int]]] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    gocList = BFTBenchmark.generateGOCList(size)
    gocLattice = BFTBenchmark.gocLattice

    bftGOCList = BFTBenchmark.generateGOCBFTList(size)
    bftGOCLattice = BFTBenchmark.bftGOCLattice

    repLL = BFTBenchmark.generateListDeltaList(size)
    repLLLattice = BFTBenchmark.dottedRepListIntLattice

    bftRepLL = BFTBenchmark.generateBFTListDeltaList(size)
    bftRepLLLattice = BFTBenchmark.latticeBFTListDeltaList
  }
}

@Fork(value = 1, warmups = 0)
@Warmup(iterations = 2)
@Measurement(iterations = 2)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class BFTBenchmark {

  @Benchmark
  def baselineGOC(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.gocList.reduce((left, right) => state.gocLattice.merge(left, right)))
  }

  @Benchmark
  def bftGOC(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.bftGOCList.reduce((left, right) => state.bftGOCLattice.merge(left, right)).value)
  }

  @Benchmark
  def baselineRepList(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.repLL.reduce((left, right) => state.repLLLattice.merge(left, right)))
  }

  @Benchmark
  def bftRepList(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.bftRepLL.reduce((left, right) => state.bftRepLLLattice.merge(left, right)).value)
  }

}

object BFTBenchmark {
  def generateGOCList(size: Int): List[GrowOnlyCounter] = {
    val id1 = LocalUid.gen()

    var goc = summon[Bottom[GrowOnlyCounter]].empty

    var list = List.empty[GrowOnlyCounter]

    list +:= goc

    for _ <- 0 to size do {
      goc = goc.inc()(using id1)
      list +:= goc
    }

    list
  }

  def gocBottom: Bottom[GrowOnlyCounter]   = summon[Bottom[GrowOnlyCounter]]
  def gocLattice: Lattice[GrowOnlyCounter] = summon[Lattice[GrowOnlyCounter]]

  def generateGOCBFTList(size: Int): List[BFT[GrowOnlyCounter]] = {
    val id1 = LocalUid.gen()

    var bft = BFT(summon[Bottom[GrowOnlyCounter]].empty)(using byteableGOC)

    var list = List.empty[BFT[GrowOnlyCounter]]

    list +:= bft

    for _ <- 0 to size do {
      bft = bft.update(_.inc()(using id1))(using byteableGOC, gocLattice, gocBottom)
      list +:= bft
    }

    list
  }

  def byteableGOC: Byteable[GrowOnlyCounter]       = it => it.inner.toString.getBytes
  def bftGOCLattice: Lattice[BFT[GrowOnlyCounter]] = BFT.lattice(using byteableGOC)

  def generateListDeltaList(size: Int): List[ReplicatedList[Int]] = {
    val id1 = LocalUid.gen()

    var repList = summon[Bottom[ReplicatedList[Int]]].empty

    var list = List.empty[ReplicatedList[Int]]

    list +:= repList

    for i <- 0 to size do {
      repList = repList.insert(0, i)(using id1)
      list +:= repList
    }

    list
  }

  def dottedRepListIntLattice: Lattice[ReplicatedList[Int]] = summon[Lattice[ReplicatedList[Int]]]

  def generateBFTListDeltaList(size: Int): List[BFT[ReplicatedList[Int]]] = {
    val id1 = LocalUid.gen()

    var bft = BFT(bottomListDeltaList.empty)(using byteableListDeltaList)

    var list = List.empty[BFT[ReplicatedList[Int]]]

    list +:= bft

    for i <- 0 to size do {
      bft = bft.update(_.insert(0, i)(using id1))(using
        byteableListDeltaList,
        dottedRepListIntLattice,
        bottomListDeltaList
      )
      list +:= bft
    }

    list
  }

  def bottomListDeltaList: Bottom[ReplicatedList[Int]]           = summon
  def byteableListDeltaList: Byteable[ReplicatedList[Int]]       = Byteable.toStringBased
  def latticeBFTListDeltaList: Lattice[BFT[ReplicatedList[Int]]] =
    BFT.lattice(using byteableListDeltaList)

}
