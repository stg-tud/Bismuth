package rdts.datatypes

import rdts.base.*
import rdts.datatypes.ReplicatedList.headDot
import rdts.time.{CausalTime, Dot, Dots}

import scala.collection.mutable.ArrayBuffer

case class ReplicatedList[E](
    causalOrder: Map[Dot, Dots],
    elements: Map[Dot, E],
    times: Map[Dot, CausalTime],
    removed: Dots
) {
  lazy val now: Option[CausalTime] = times.valuesIterator.reduceOption(Lattice.merge)
  lazy val observed: Dots          = removed.union(Dots.from(elements.keys))

  def nextTime: CausalTime = now.map(_.advance).getOrElse(CausalTime.now())

  def insertAfter(predecessor: Dot, values: Iterable[E])(using LocalUid): ReplicatedList[E] = {
    val nextDots  = Iterable.iterate(observed.nextDot, values.size)(_.advance)
    val nextTimes = Iterable.iterate(nextTime, values.size)(_.advance)

    ReplicatedList(
      causalOrder = Map(predecessor -> Dots.from(nextDots)),
      elements = Map.from(nextDots `zip` values),
      times = Map.from(nextDots `zip` nextTimes),
      removed = Dots.empty
    )

  }

  def findOptimizedInsertionPoint(predecessor: Dot): Dot = {
    def standardResult = predecessor
    return standardResult
    if !causalOrder.getOrElse(predecessor, Dots.empty).isEmpty then standardResult
    else {
      causalOrder.collectFirst { case (pred, succs) if succs.contains(predecessor) => (pred, succs) } match {
        case None                     => standardResult
        case Some((greatPred, succs)) =>
          val sorted = succs.iterator.toSeq.sortBy(d => times(d))
          if !sorted.lastOption.contains(predecessor) then standardResult
          else {
            greatPred
          }
      }
    }
  }

  lazy val toposort: Seq[Dot] = {
    val sorted               = ArrayBuffer[Dot]()
    var discovered: Set[Dot] = Set.empty

    def _toposort(rem: Dot): Unit = {
      if discovered.contains(rem) then ()
      else {
        discovered = discovered + rem
        val next =
          causalOrder.getOrElse(rem, Dots.empty).iterator.flatMap { d =>
            times.get(d).map(t => d -> t)
          }.toSeq.sortBy((d, t) => t).map(_._1)
        next.foreach(_toposort)
        sorted += rem
        ()
      }
    }

    _toposort(headDot)
    sorted.toSeq.reverse
  }

  lazy val dotList: Seq[Dot] = {
    val res = toposort.filterNot(removed.contains)
    assert(res.sizeIs == (size + 1))
    res
  }

  lazy val toList: List[E] = dotList.flatMap(elements.get).toList

  def toLazyList: LazyList[E] = toList.to(LazyList)

  def read(i: Int): Option[E] = toList.lift(i)

  def insert(index: Int, elem: E)(using LocalUid): ReplicatedList[E]   = insertAt(index, elem)
  def insertAt(index: Int, elem: E)(using LocalUid): ReplicatedList[E] = {
    val pos = findOptimizedInsertionPoint(dotList(index))
    insertAfter(pos, Iterable(elem))
  }

  def insertAll(index: Int, elems: Iterable[E])(using LocalUid): ReplicatedList[E] = {
    val pos = findOptimizedInsertionPoint(dotList(index))
    insertAfter(pos, elems)

  }

  def delete(index: Int): ReplicatedList[E]      = removeIndex(index)
  def removeIndex(index: Int): ReplicatedList[E] = {

    if index < 0 || dotList.sizeIs <= index + 1 then ReplicatedList.empty
    else
      ReplicatedList(
        causalOrder = Map.empty,
        elements = Map.empty,
        removed = Dots.single(dotList(index + 1)),
        times = Map.empty
      )
  }

  def size: Int = elements.size

  def compact: ReplicatedList[E] = {
    copy(
      elements = elements.filterNot((d, _) => removed.contains(d)),
    )
  }

  def appendAll(elements: Iterable[E])(using LocalUid): ReplicatedList[E] = {
    val pos = findOptimizedInsertionPoint(dotList.lastOption.getOrElse(ReplicatedList.headDot))
    insertAfter(pos, elements)
  }

  def prependAll(e: Iterable[E])(using LocalUid): ReplicatedList[E] = insertAfter(headDot, e)
  def prepend(e: E)(using LocalUid): ReplicatedList[E]              = insertAfter(dotList(0), List(e))
  def append(e: E)(using LocalUid): ReplicatedList[E]               =
    val pos = findOptimizedInsertionPoint(dotList.lastOption.getOrElse(ReplicatedList.headDot))
    insertAfter(pos, List(e))

  def update(index: Int, elem: E)(using LocalUid): ReplicatedList[E] = {
    val pos = dotList(index + 1)
    insertAfter(pos, List(elem)).copy(removed = Dots.single(pos))
  }

  def deleteBy(test: E => Boolean): ReplicatedList[E] = {
    val toRemove = elements.filter((dot, e) => test(e)).keys
    ReplicatedList(
      Map.empty,
      Map.empty,
      Map.empty,
      Dots.from(toRemove)
    )
  }
}

object ReplicatedList {
  val headDot = Dot.zero

  given decompose[E]: Decompose[ReplicatedList[E]] = {
    given Decompose[E]          = Decompose.atomic
    given Decompose[CausalTime] = Decompose.atomic
    Decompose.derived
  }

  given bottom[E]: Bottom[ReplicatedList[E]]   = Bottom.derived
  def empty[E]: ReplicatedList[E]              = bottom.empty
  given lattice[E]: Lattice[ReplicatedList[E]] = {
    val base: Lattice[ReplicatedList[E]] = {
      given Lattice[E] = Lattice.assertEquals
      Lattice.derived
    }
    DecoratedLattice.compact(base) { _.compact }
  }
}
