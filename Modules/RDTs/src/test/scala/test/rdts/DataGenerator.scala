package test.rdts

import org.scalacheck.{Arbitrary, Gen, Shrink}
import rdts.base.*
import rdts.datatypes.*
import rdts.experiments.AutomergyOpGraphLWW.OpGraph
import rdts.experiments.CausalStore
import rdts.experiments.CausalStore.CausalDelta
import rdts.time.*

import scala.annotation.{nowarn, tailrec}

object DataGenerator {

  case class ExampleData(content: Set[String]) derives Lattice, Bottom {
    override def toString: _root_.java.lang.String = content.mkString("\"", "", "\"")
  }
  object ExampleData:
    given Conversion[String, ExampleData] = ed => ExampleData(Set(ed))

  given Arbitrary[ExampleData] = Arbitrary:
    Gen.oneOf(List("Anne", "Ben", "Chris", "Erin", "Julina", "Lynn", "Sara", "Taylor")).map(name =>
      ExampleData(Set(name))
    )

  given arbId: Arbitrary[Uid] = Arbitrary(Gen.oneOf('a' to 'g').map(c => Uid.predefined(c.toString)))

  given arbVectorClock: Arbitrary[VectorClock] = Arbitrary:
    for
      ids: Set[Uid]     <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
      value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
    yield VectorClock.fromMap(ids.zip(value).toMap)

  val smallNum: Gen[Int] = Gen.chooseNum(-10, 10)

  given arbCausalTime: Arbitrary[CausalTime] = Arbitrary:
    for
      time     <- smallNum
      causal   <- smallNum
      nanotime <- Gen.long
    yield CausalTime(time, causal, nanotime)

  given arbLww[E: Arbitrary]: Arbitrary[LastWriterWins[E]] = Arbitrary:
    for
      causal <- arbCausalTime.arbitrary
      value  <- Arbitrary.arbitrary
    yield LastWriterWins(causal, value)

  given arbGcounter: Arbitrary[GrowOnlyCounter] = Arbitrary(
    Gen.mapOf[Uid, Int](Gen.zip(arbId.arbitrary, smallNum)).map(GrowOnlyCounter(_))
  )

  given arbPosNeg: Arbitrary[PosNegCounter] = Arbitrary(
    for
      pos <- arbGcounter.arbitrary
      neg <- arbGcounter.arbitrary
    yield PosNegCounter(pos, neg)
  )

  given Lattice[Int] = _ max _

  val genDot: Gen[Dot] =
    for
      id    <- Gen.oneOf('a' to 'g')
      value <- Gen.oneOf(0 to 100)
    yield Dot(Uid.predefined(id.toString), value)

  val uniqueDot: Gen[Dot] =
    for
      id    <- Gen.oneOf('a' to 'g')
      value <- Gen.long
    yield Dot(Uid.predefined(id.toString), value)

  given arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  given arbDots: Arbitrary[Dots] = Arbitrary:
    Gen.containerOfN[Set, Dot](10, genDot).map(Dots.from)

  given Arbitrary[ArrayRanges] = Arbitrary(
    for
      x <- Gen.listOf(smallNum)
    yield ArrayRanges.from(x.map(_.toLong))
  )

  def genDotFun[A](using g: Arbitrary[A]): Gen[Map[Dot, A]] =
    for
      n      <- Gen.choose(0, 10)
      dots   <- Gen.containerOfN[List, Dot](n, genDot)
      values <- Gen.containerOfN[List, A](n, g.arbitrary)
    yield (dots zip values).toMap

  given arbDotFun[A](using g: Arbitrary[A]): Arbitrary[Map[Dot, A]] = Arbitrary(genDotFun)

  @tailrec
  def makeUnique(rem: List[Dots], acc: List[Dots], state: Dots): List[Dots] =
    rem match
      case Nil    => acc
      case h :: t => makeUnique(t, h.subtract(state) :: acc, state `union` h)

  case class SmallTimeSet(s: Set[Time])

  given Arbitrary[SmallTimeSet] = Arbitrary(for
    contents <- Gen.listOf(Gen.chooseNum(0L, 100L))
  yield SmallTimeSet(contents.toSet))

  given arbGrowOnlyList[E](using arb: Arbitrary[E]): Arbitrary[GrowOnlyList[E]] = Arbitrary:
    Gen.listOf(arb.arbitrary).map: list =>
      GrowOnlyList.empty.insertAllAt(0, list)


  given arbDotmap[K, V](using arbElem: Arbitrary[K], arbKey: Arbitrary[V]): Arbitrary[Map[K, V]] =
    Arbitrary:
      Gen.listOf(Gen.zip[K, V](arbElem.arbitrary, arbKey.arbitrary)).map: pairs =>
        pairs.toMap

  given arbCMultiVersion[E](using arb: Arbitrary[E]): Arbitrary[MultiVersionRegister[E]] = Arbitrary {
    for
      elements <- Gen.listOf(Gen.zip(uniqueDot, arb.arbitrary))
      removed  <- arbDots.arbitrary
    yield MultiVersionRegister(elements.toMap, removed)
  }

  given arbEnableWinsFlag: Arbitrary[EnableWinsFlag] = Arbitrary {
    for
      set   <- arbDots.arbitrary
      unset <- arbDots.arbitrary
    yield EnableWinsFlag(set, unset)
  }

  given arbCausalDelta[A: {Arbitrary}]: Arbitrary[CausalDelta[A]] = Arbitrary:
    for
      predec <- arbDots.arbitrary
      value  <- Arbitrary.arbitrary[A]
      dots   <- arbDot.arbitrary
    yield CausalDelta(dots, Dots.empty, value)

  given arbCausalStore[A: {Arbitrary, Bottom, Lattice}]: Arbitrary[CausalStore[A]] = Arbitrary:
    for
      predec <- Gen.listOf(arbCausalDelta.arbitrary)
      value  <- Arbitrary.arbitrary[A]
      dots   <- arbDots.arbitrary
    yield Lattice.normalize(CausalStore(predec.toSet, dots, Some(value)))

  object RGAGen {
    def makeRGA[E](
        inserted: List[(Int, E)],
        removed: List[Int],
        rid: LocalUid
    ): ReplicatedList[E] = {
      val afterInsert = inserted.foldLeft(ReplicatedList.empty[E]) {
        case (rga, (i, e)) => rga `merge` rga.insert(i, e)(using rid)
      }

      removed.foldLeft(afterInsert) {
        case (rga, i) => rga `merge` rga.delete(i)
      }
    }

    def genRGA[E](using e: Arbitrary[E]): Gen[ReplicatedList[E]] =
      for
        nInserted       <- Gen.choose(0, 20)
        insertedIndices <- Gen.containerOfN[List, Int](nInserted, Arbitrary.arbitrary[Int])
        insertedValues  <- Gen.containerOfN[List, E](nInserted, e.arbitrary)
        removed         <- Gen.containerOf[List, Int](Arbitrary.arbitrary[Int])
        id              <- Gen.oneOf('a' to 'g')
      yield {
        makeRGA(insertedIndices zip insertedValues, removed, Uid.predefined(id.toString).convert)
      }

    given arbRGA[E](using
        e: Arbitrary[E],
    ): Arbitrary[ReplicatedList[E]] =
      Arbitrary(genRGA)

  }

  given arbOpGraph[T](using arbData: Arbitrary[T]): Arbitrary[OpGraph[T]] = Arbitrary:
    Gen.containerOf[List, T](arbData.arbitrary).map: elems =>
      elems.foldLeft(OpGraph.bottom.empty): (curr, elem) =>
        curr `merge` curr.set(elem)

}
