package rdts.datatypes

import rdts.time.Dot
import rdts.time.Dots
import rdts.datatypes.LastWriterWins as LWW
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.DecoratedLattice
import rdts.base.Uid

case class RemoveWinsArray[E](
    elements: Map[Dot, RemoveWinsArray.Entry[E]],
    removed: Dots
) {
  lazy val observed: Dots = removed.union(Dots.from(elements.keys))

  lazy val entries: List[(Dot, RemoveWinsArray.Entry[E])] = {
    compactElements.toList.sortBy(e => e._2.index.value)
  }

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

  def append(elem: E)(using LocalUid) = insert(size, elem)

  def insert(index: Int, elem: E)(using LocalUid): RemoveWinsArray[E] = {
    val dot = observed.nextDot

    val entriesList = entries
    val beforePos   = entriesList.lift(index - 1).map(_._2.index.value).getOrElse(LSeq.min)
    val afterPos    = entriesList.lift(index).map(_._2.index.value).getOrElse(LSeq.max)

    val pos = LSeq.between(beforePos, afterPos, LocalUid.replicaId)

    val entry = RemoveWinsArray.Entry(LWW.now(pos), elem)

    RemoveWinsArray(elements + (dot -> entry), removed)
  }

  def update(index: Int, elem: E)(using LocalUid): RemoveWinsArray[E] = {
    entries.lift(index) match {
      case Some((oldDot, _)) =>
        val dot   = observed.nextDot
        val entry = RemoveWinsArray.Entry(LWW.now(entries(index)._2.index.value), elem)
        RemoveWinsArray(elements = Map(dot -> entry), removed = Dots.single(oldDot))
      case None => insert(index, elem)
    }
  }

  def remove(index: Int): RemoveWinsArray[E] = {
    entries.lift(index) match {
      case Some((dot, _)) => RemoveWinsArray(elements = Map(), removed = removed.add(dot))
      case None           => RemoveWinsArray.empty
    }
  }

  def move(from: Int, to: Int)(using LocalUid): RemoveWinsArray[E] = {
    if from < 0 || to < 0 || from >= size || to >= size then RemoveWinsArray.empty
    else if from == to then RemoveWinsArray.empty
    else
      val entriesList = entries
      entriesList.lift(from) match {
        case Some((dot, entry)) =>
          val pos = {
            val beforePos = entriesList.lift(to).map(_._2.index.value).getOrElse(LSeq.min)
            val afterPos  = entriesList.lift(to + 1).map(_._2.index.value).getOrElse(LSeq.max)
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
    given lattice[A]: Lattice[Entry[A]] = {
      given Lattice[A] = Lattice.assertEquals
      Lattice.derived
    }
  }

  def empty[A]: RemoveWinsArray[A] = RemoveWinsArray(Map.empty, Dots.empty)

  given lattice[E]: Lattice[RemoveWinsArray[E]] = {
    val base: Lattice[RemoveWinsArray[E]] = {
      given Lattice[E] = Lattice.assertEquals
      Lattice.derived
    }
    DecoratedLattice.compact(base) { _.compact }
  }
}

// An LSEQ position identifier is an ordered list of components.
type LSeq = List[LSeq.Component]

object LSeq {
  case class Component(position: Int, place: Uid)

  def min: LSeq = List(Component(0, Uid.zero))
  def max: LSeq = List(Component(Int.MaxValue, Uid.predefined("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"))) // TODO

  def between(left: LSeq, right: LSeq, place: Uid): LSeq = {
    val commonPrefix = left.zip(right).takeWhile { case (l, r) => l.position == r.position }
    val prefixLength = commonPrefix.length

    val leftComponent  = left.drop(prefixLength).headOption
    val rightComponent = right.drop(prefixLength).headOption

    val lowerBound = leftComponent.map(_.position).getOrElse(0)
    val upperBound = rightComponent.map(_.position).getOrElse(Int.MaxValue)

    // Try to insert a new position *at the current depth* (LSEQ's core optimization).
    if upperBound - lowerBound > 1 then {
      // There is integer space at the current depth.
      // Choose a value in the middle to leave gaps for future insertions.
      val newPosition  = lowerBound + ((upperBound - lowerBound) / 2)
      val newComponent = LSeq.Component(newPosition, place)

      commonPrefix.map(_._1) ++ List(newComponent)
    } else {
      // No integer space left. We must increase the depth (Logoot-like splitting).
      // This means the new position will be a new component appended to the 'left' ID.
      // The new position is generated in the gap between the last component of 'left'
      // and the start of the next depth.

      // Use the entire left ID as the new prefix.
      val newPrefix = left

      // Choose a new position (e.g., a constant value like 5, or use a more
      // sophisticated LSEQ allocation strategy to create a new gap at this new depth).
      // Let's use 5 as a simplistic new position at the next depth.
      val newComponent = LSeq.Component(5, place)

      // The new ID is the entire left ID + the new component.
      newPrefix ++ List(newComponent)
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
