package rdts.datatypes

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

case class RemoveWinsArray[E](
    elements: Map[Dot, RemoveWinsArray.Entry[E]],
    removed: Dots
) {
  lazy val observed: Dots = removed.union(Dots.from(elements.keys))

  lazy val entries: List[(Dot, RemoveWinsArray.Entry[E])] =
    compactElements.toList.sortBy(e => e._2.index.value)

  lazy val compactElements: Map[Dot, RemoveWinsArray.Entry[E]] =
    elements.filterNot((d, _) => removed.contains(d))

  lazy val toList: List[E] = entries.map(e => e._2.value)

  def compact: RemoveWinsArray[E] = {
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

  def insert(index: Int, value: E)(using LocalUid): RemoveWinsArray[E] =
    insertAll(index, Iterable(value))

  def insertAll(index: Int, values: Iterable[E])(using LocalUid): RemoveWinsArray[E] = {
    val nextDots = Iterable.iterate(observed.nextDot, values.size)(_.advance)

    val entriesList = entries
    var beforePos   = entriesList.lift(index - 1).map(_._2.index.value).getOrElse(LSeq.min)
    val afterPos    = entriesList.lift(index).map(_._2.index.value).getOrElse(LSeq.max)

    val timestamp = CausalTime.now()

    val newElements = scala.collection.mutable.Map[Dot, RemoveWinsArray.Entry[E]]()
    for (value, dot) <- values.zip(nextDots) do
        val newPos = LSeq.between(beforePos, afterPos, LocalUid.replicaId)
        newElements += (dot -> RemoveWinsArray.Entry(LWW(timestamp, newPos), value))
        beforePos = newPos

    RemoveWinsArray(elements = newElements.toMap, removed)
  }

  def update(index: Int, elem: E): RemoveWinsArray[E] =
    updateWith(index, _ => elem)

  def updateWith(index: Int, f: E => E): RemoveWinsArray[E] = {
    entries.lift(index) match {
      case Some((dot, oldEntry)) =>
        val entry = RemoveWinsArray.Entry(LWW.now(oldEntry.index.value), f(oldEntry.value))
        RemoveWinsArray(elements = Map(dot -> entry), removed = Dots.empty)
      case None => RemoveWinsArray.empty
    }
  }

  def remove(index: Int): RemoveWinsArray[E] = {
    entries.lift(index) match {
      case Some((dot, _)) => RemoveWinsArray(elements = Map(), removed = removed.add(dot))
      case None           => RemoveWinsArray.empty
    }
  }

  def move(from: Int, to: Int)(using LocalUid): RemoveWinsArray[E] = {
    if from < 0 || to < 0 || from > size || to > size then RemoveWinsArray.empty
    else if from == to then RemoveWinsArray.empty
    else
        val entriesList = entries
        entriesList.lift(from) match {
          case Some((dot, entry)) =>
            val pos = {
              val beforePos = entriesList.lift(to - 1).map(_._2.index.value).getOrElse(LSeq.min)
              val afterPos  = entriesList.lift(to).map(_._2.index.value).getOrElse(LSeq.max)
              LSeq.between(beforePos, afterPos, LocalUid.replicaId)
            }
            RemoveWinsArray(
              elements = Map(dot -> entry.copy(
                index = LWW.now(pos)
              )),
              removed = Dots.empty
            )
          case None => RemoveWinsArray.empty
        }
  }

  def moveRange(fromStart: Int, fromEnd: Int, toIndex: Int)(using LocalUid): RemoveWinsArray[E] = {
    if fromStart < 0 || fromEnd < 0 || toIndex < 0 then RemoveWinsArray.empty
    else if fromStart >= fromEnd then RemoveWinsArray.empty
    else if fromStart >= size || toIndex > size then RemoveWinsArray.empty
    else if fromEnd > size then RemoveWinsArray.empty
    else
        val entriesList    = entries
        val elementsToMove = entriesList.slice(fromStart, fromEnd)

        if elementsToMove.isEmpty then RemoveWinsArray.empty
        else
            val beforePos = entriesList.lift(toIndex - 1).map(_._2.index.value).getOrElse(LSeq.min)
            val afterPos  = entriesList.lift(toIndex).map(_._2.index.value).getOrElse(LSeq.max)

            val newElements = scala.collection.mutable.Map[Dot, RemoveWinsArray.Entry[E]]()
            var currentPos  = beforePos

            for (dot, entry) <- elementsToMove do
                val newPos = LSeq.between(currentPos, afterPos, LocalUid.replicaId)
                newElements += (dot -> entry.copy(index = LWW.now(newPos)))
                currentPos = newPos

            RemoveWinsArray(
              elements = newElements.toMap,
              removed = Dots.empty
            )
  }

  def clear(): RemoveWinsArray[E] = {
    RemoveWinsArray(
      elements = Map.empty,
      removed = observed
    )
  }
}

object RemoveWinsArray {
  case class Entry[A](index: LWW[LSeq], value: A)

  object Entry {
    given lattice[A: Lattice]: Lattice[Entry[A]] = Lattice.derived

    given decompose[E]: Decompose[Entry[E]] = {
      given Decompose[E] = Decompose.atomic
      Decompose.derived
    }
  }

  def empty[A]: RemoveWinsArray[A] = RemoveWinsArray(Map.empty, Dots.empty)

  def of[A](values: A*)(using LocalUid): RemoveWinsArray[A] = {
    values.foldLeft(RemoveWinsArray.empty[A]) { (arr, v) =>
      arr.append(v)
    }
  }

  given decompose[E]: Decompose[RemoveWinsArray[E]] = Decompose.derived

  given lattice[E: Lattice]: Lattice[RemoveWinsArray[E]] = {
    val base: Lattice[RemoveWinsArray[E]] = Lattice.derived
    DecoratedLattice.compact(base) { _.compact }
  }

  given bottom[E]: Bottom[RemoveWinsArray[E]] = Bottom.provide(empty)
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
