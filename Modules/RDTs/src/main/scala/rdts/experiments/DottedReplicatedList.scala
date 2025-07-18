package rdts.experiments

import rdts.base.*
import rdts.experiments.DottedReplicatedList.headDot
import rdts.time.{CausalTime, Dot, Dots}

import scala.collection.mutable.ArrayBuffer

case class DottedReplicatedList[E](
    causalOrder: Map[Dot, Dots],
    elements: Map[Dot, E],
    times: Map[Dot, CausalTime],
    removed: Dots
) {
  lazy val now: Option[CausalTime] = times.valuesIterator.reduceOption(Lattice.merge)
  lazy val observed: Dots          = removed.union(Dots.from(elements.keys))

  def nextTime: CausalTime = now.map(_.advance).getOrElse(CausalTime.now())

  def insertAfter(predecessor: Dot, values: Iterable[E])(using LocalUid): DottedReplicatedList[E] = {
    val nextDots  = Iterable.iterate(observed.nextDot, values.size)(_.advance)
    val nextTimes = Iterable.iterate(nextTime, values.size)(_.advance)

    DottedReplicatedList(
      causalOrder = Map(predecessor -> Dots.from(nextDots)),
      elements = Map.from(nextDots `zip` values),
      times = Map.from(nextDots `zip` nextTimes),
      removed = Dots.empty
    )

  }

  def findOptimizedInsertionPoint(predecessor: Dot): Dot = {
    def standardResult = predecessor
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
        val next = causalOrder.getOrElse(rem, Dots.empty).iterator.toSeq.sortBy(d => times(d)).reverse
        next.foreach(_toposort)
        sorted += rem
        ()
      }
    }

    (Iterable(
      DottedReplicatedList.headDot
    ) ++ causalOrder.keys.toSeq.sorted(using Orderings.lexicographic[Dot])).foreach(_toposort)
    sorted.toSeq.reverse
  }

  lazy val dotList: Seq[Dot] = toposort

  lazy val toList: List[E] = dotList.flatMap(elements.get).toList

  def toLazyList: LazyList[E] = toList.to(LazyList)

  def read(i: Int): Option[E] = toList.lift(i)

  def insertAt(index: Int, elem: E)(using LocalUid) = {
    val pos = findOptimizedInsertionPoint(dotList(index))
    insertAfter(pos, Iterable(elem))
  }

  def insertAllAt(index: Int, elems: List[E])(using LocalUid): DottedReplicatedList[E] = {
    val pos = findOptimizedInsertionPoint(dotList(index))
    insertAfter(pos, elems)

  }

  def removeIndex(index: Int): DottedReplicatedList[E] = {
    DottedReplicatedList(
      causalOrder = Map.empty,
      elements = Map.empty,
      removed = Dots.single(dotList(index)),
      times = Map.empty
    )
  }

  def size: Int = elements.size

  def compact: DottedReplicatedList[E] = {
    copy(
      elements = elements.filterNot((d, _) => removed.contains(d)),
      times = times.filterNot((d, _) => removed.contains(d)),
    )
  }

  def appendAll(elements: Iterable[E])(using LocalUid): DottedReplicatedList[E] = {
    val pos = findOptimizedInsertionPoint(dotList.lastOption.getOrElse(DottedReplicatedList.headDot))
    insertAfter(pos, elements)
  }

  def prependAll(e: Iterable[E])(using LocalUid) = insertAfter(headDot, e)
  def prepend(e: E)(using LocalUid) = insertAfter(headDot, List(e))
  def append(e: E)(using LocalUid) =
    val pos = findOptimizedInsertionPoint(dotList.lastOption.getOrElse(DottedReplicatedList.headDot))
    insertAfter(pos, List(e))

}

object DottedReplicatedList {
  val headDot = Dot.zero

  given decompose[E]: Decompose[DottedReplicatedList[E]] = {
    given Decompose[E]          = Decompose.atomic
    given Decompose[CausalTime] = Decompose.atomic
    Decompose.derived
  }

  given bottom[E]: Bottom[DottedReplicatedList[E]]   = Bottom.derived
  def empty[E]: DottedReplicatedList[E]              = bottom.empty
  given lattice[E]: Lattice[DottedReplicatedList[E]] = {
    val base: Lattice[DottedReplicatedList[E]] = {
      given Lattice[E] = Lattice.assertEquals
      Lattice.derived
    }
    DecoratedLattice.compact(base) { _.compact }
  }
}
