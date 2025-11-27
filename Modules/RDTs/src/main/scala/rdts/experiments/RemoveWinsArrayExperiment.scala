package rdts.experiments

import rdts.time.Dot
import rdts.time.Dots
import rdts.datatypes.LastWriterWins as LWW
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.DecoratedLattice
import rdts.base.Uid
import rdts.base.Bottom
import rdts.time.CausalTime
import rdts.base.Decompose

case class RemoveWinsArrayExperiment[E](
    elements: Map[Dot, RemoveWinsArrayExperiment.Entry[E]],
    removed: Dots,
    ops: Map[Dot, E => E] = Map.empty,
    history: Map[Dot, Dots] = Map.empty,
) {
  lazy val observed: Dots = removed.union(Dots.from(elements.keys)).union(Dots.from(ops.keys))

  lazy val entries: List[(Dot, RemoveWinsArrayExperiment.Entry[E])] = {
    compactElements.toList.sortBy(e => e._2.index.value)
  }

  lazy val compactElements: Map[Dot, RemoveWinsArrayExperiment.Entry[E]] =
    elements.filterNot((d, _) => removed.contains(d))

  lazy val toList: List[E] = {
    val e                                              = entries
    val output: scala.collection.mutable.ListBuffer[E] =
      e.map(_._2.value).to(scala.collection.mutable.ListBuffer)

    for ((itemId, item), ix) <- e.zipWithIndex do {
      for (opId, apply) <- ops do {
        val ordering = Dots.partialOrder.tryCompare(history(itemId), history(opId))
        ordering match {
          // item inserted before or at same time as operation
          case Some(i) if i <= 0 => output.update(ix, apply(output(ix)))
          // item inserted concurrently with operation
          case None => output.update(ix, apply(output(ix)))
          case _    => () // item inserted after operation
        }
      }
    }
    output.toList
  }

  def compact: RemoveWinsArrayExperiment[E] = {
    copy(
      elements = compactElements,
    )
  }

  def read(i: Int): Option[E] = toList.lift(i)

  def size: Int = compactElements.size

  def prepend(value: E)(using LocalUid)               = insert(0, value)
  def prependAll(values: Iterable[E])(using LocalUid) = insertAll(0, values)

  def append(value: E)(using LocalUid)               = insert(size, value)
  def appendAll(values: Iterable[E])(using LocalUid) = insertAll(size, values)

  def insert(index: Int, value: E)(using LocalUid): RemoveWinsArrayExperiment[E] = {
    insertAll(index, Iterable(value))
  }

  def insertAll(index: Int, values: Iterable[E])(using LocalUid): RemoveWinsArrayExperiment[E] = {
    val nextDots = Iterable.iterate(observed.nextDot, values.size)(_.advance)

    val entriesList = entries
    var beforePos   = entriesList.lift(index - 1).map(_._2.index.value).getOrElse(LSeq.min)
    val afterPos    = entriesList.lift(index).map(_._2.index.value).getOrElse(LSeq.max)

    val timestamp = CausalTime.now()

    val newElements  = scala.collection.mutable.Map[Dot, RemoveWinsArrayExperiment.Entry[E]]()
    val predecessors = observed
    for (value, dot) <- values.zip(nextDots) do
      val newPos = LSeq.between(beforePos, afterPos, LocalUid.replicaId)
      newElements += (dot -> RemoveWinsArrayExperiment.Entry(LWW(timestamp, newPos), value))
      beforePos = newPos

    RemoveWinsArrayExperiment(
      elements = newElements.toMap,
      removed,
      history = nextDots.map(dot => dot -> predecessors).toMap
    )
  }

  def update(index: Int, elem: E)(using LocalUid): RemoveWinsArrayExperiment[E] = {
    entries.lift(index) match {
      case Some((oldDot, _)) =>
        val predecessors = observed
        val dot          = predecessors.nextDot
        val entry        = RemoveWinsArrayExperiment.Entry(LWW.now(entries(index)._2.index.value), elem)
        RemoveWinsArrayExperiment(
          elements = Map(dot -> entry),
          removed = Dots.single(oldDot),
          history = Map(dot -> predecessors)
        )
      case None => insert(index, elem)
    }
  }

  def remove(index: Int): RemoveWinsArrayExperiment[E] = {
    entries.lift(index) match {
      case Some((dot, _)) => RemoveWinsArrayExperiment(elements = Map(), removed = removed.add(dot))
      case None           => RemoveWinsArrayExperiment.empty
    }
  }

  def move(from: Int, to: Int)(using LocalUid): RemoveWinsArrayExperiment[E] = {
    if from < 0 || to < 0 || from >= size || to >= size then RemoveWinsArrayExperiment.empty
    else if from == to then RemoveWinsArrayExperiment.empty
    else
      val entriesList = entries
      entriesList.lift(from) match {
        case Some((dot, entry)) =>
          val pos = {
            val beforePos = entriesList.lift(to).map(_._2.index.value).getOrElse(LSeq.min)
            val afterPos  = entriesList.lift(to + 1).map(_._2.index.value).getOrElse(LSeq.max)
            LSeq.between(beforePos, afterPos, LocalUid.replicaId)
          }
          RemoveWinsArrayExperiment(
            elements = Map(dot -> entry.copy(
              index = LWW.now(pos)
            )),
            removed = Dots.empty
          )
        case None => RemoveWinsArrayExperiment.empty
      }
  }

  def apply(fn: E => E)(using LocalUid): RemoveWinsArrayExperiment[E] = {
    val predecessors = observed
    val dot          = predecessors.nextDot
    RemoveWinsArrayExperiment(
      elements = Map.empty,
      removed = Dots.empty,
      history = Map(dot -> predecessors),
      ops = Map(dot -> fn)
    )
  }

  def clear(): RemoveWinsArrayExperiment[E] = {
    RemoveWinsArrayExperiment(
      elements = Map.empty,
      removed = observed
    )
  }
}

object RemoveWinsArrayExperiment {
  case class Entry[A](index: LWW[LSeq], value: A)

  object Entry {
    given lattice[A]: Lattice[Entry[A]] = {
      given Lattice[A] = Lattice.assertEquals
      Lattice.derived
    }

    given decompose[E]: Decompose[Entry[E]] = {
      given Decompose[E] = Decompose.atomic
      Decompose.derived
    }
  }

  def empty[A]: RemoveWinsArrayExperiment[A] = RemoveWinsArrayExperiment(Map.empty, Dots.empty)

  given lattice[E]: Lattice[RemoveWinsArrayExperiment[E]] = {
    val base: Lattice[RemoveWinsArrayExperiment[E]] = {
      given Lattice[E] = Lattice.assertEquals
      Lattice.derived
    }
    DecoratedLattice.compact(base) { _.compact }
  }

  given bottom[E]: Bottom[RemoveWinsArrayExperiment[E]] = Bottom.provide(empty)
}

type LSeq = List[LSeq.Component]

object LSeq {
  case class Component(position: Int, place: Uid)

  def min: LSeq = List(Component(0, Uid.zero))
  def max: LSeq = List(Component(Int.MaxValue, Uid.predefined("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"))) // TODO

  def between(left: LSeq, right: LSeq, place: Uid): LSeq = {
    val commonPrefix           = left.zip(right).takeWhile { case (l, r) => l.position == r.position }
    val commonPrefixComponents = commonPrefix.map(_._1)

    val lowerBound = left.drop(commonPrefix.length).headOption.map(_.position).getOrElse(0)
    val upperBound = right.drop(commonPrefix.length).headOption.map(_.position).getOrElse(Int.MaxValue)

    if upperBound - lowerBound > 1 then {
      val newPosition = lowerBound + ((upperBound - lowerBound) / 2)
      commonPrefixComponents ++ List(LSeq.Component(newPosition, place))
    } else {
      left ++ List(LSeq.Component(5, place))
    }
  }

  implicit val componentOrdering: Ordering[LSeq.Component] = Ordering.fromLessThan { (a, b) =>
    if a.position != b.position then a.position < b.position
    else Uid.ordering.compare(a.place, b.place) < 0
  }

  implicit val PositionOrdering: Ordering[LSeq] = new Ordering[LSeq] {
    def compare(id1: LSeq, id2: LSeq): Int = {
      val zipped = id1.zip(id2)

      val difference = zipped.collectFirst {
        case (c1, c2) if componentOrdering.compare(c1, c2) != 0 =>
          componentOrdering.compare(c1, c2)
      }

      difference match {
        case Some(diff) =>
          // Found a difference (lexicographical comparison wins).
          diff
        case None =>
          // No differences found up to the length of the shorter list.
          // This means one is a prefix of the other, or they are identical.
          // The shorter list comes first.
          id1.length compare id2.length
      }
    }
  }
}
