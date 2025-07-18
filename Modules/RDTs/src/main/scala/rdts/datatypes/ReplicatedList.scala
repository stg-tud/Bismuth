package rdts.datatypes

import rdts.base.{Bottom, Decompose, DecoratedLattice, Lattice, LocalUid}
import rdts.datatypes.{Epoch, GrowOnlyList, LastWriterWins}
import rdts.time.{Dot, Dots}

/** An RGA (Replicated Growable Array) is a Delta CRDT modeling a list.
  *
  * When two values are concurrently inserted at an index i, the value of the insert operation with the later timestamp
  * will be at index i while the earlier inserted value will be pushed to index i+1. When an element is subject to two
  * concurrent updates, the later update overwrites the earlier update. If an element is concurrently updated and deleted,
  * the element will simply be deleted, ignoring the update.
  *
  * Note that RGAs are implemented as linked lists, thus the time needed to execute operations toward the end of the list
  * will scale linearly with the size of the list.
  *
  * To correctly handle concurrent remote inserts next to elements that were deleted locally, the RGA implementation internally
  * keeps deleted elements as hidden tombstones in the list. Since many tombstones will slow down the operations on this
  * data structure, purgeTombstones should be executed periodically to remove tombstones from the list. Note however that
  * this invalidates any concurrent insert operations. Ideally, purgeTombstones should only be called in downtime periods
  * and only by privileged replicas.
  *
  * This implementation was modeled after the RGA proposed by Roh et al. in "Replicated abstract data types: Building blocks
  * for collaborative applications", see [[https://www.sciencedirect.com/science/article/pii/S0743731510002716?casa_token=lQaLin7aEvcAAAAA:Esc3h3WvkFHUcvhalTPPvV5HbJge91D4-2jyKiSlz8GBDjx31l4xvfH8DIstmQ973PVi46ckXHg here]]
  * However, since then the implementation was changed significantly, thus it may be a different or even a novel strategy by now.
  */
case class ReplicatedList[E](
    order: Epoch[GrowOnlyList[Dot]] = Epoch.empty,
    elements: Map[Dot, LastWriterWins[E]] = Map.empty,
    deleted: Dots = Dots.empty
) {

  def read(i: Int): Option[E] = {
    order.value.toLazyList.flatMap(elements.get).map(_.payload).lift(i)
  }

  def sizeIncludingDeadElements: Int = elements.size

  def size: Int = toList.size

  def toList: List[E] = {
    order.value.toList.flatMap(elements.get).map(_.payload)
  }

  def findInsertIndex(n: Int): Option[Int] = {
    order.value.toLazyList.zip(LazyList.from(1)).filter {
      case (dot, _) => elements.contains(dot)
    }.map(_._2).prepended(0).lift(n)
  }

  lazy val observed: Dots = deleted.union(Dots.from(elements.keys))

  /** Inserts at list index `i` */
  def insert(i: Int, e: E)(using LocalUid): ReplicatedList[E] = {
    val nextDot = observed.nextDot(LocalUid.replicaId)

    findInsertIndex(i) match {
      case None                   => ReplicatedList.empty[E]
      case Some(glistInsertIndex) =>
        val glistDelta = order.map { gl =>
          gl.insertAt(glistInsertIndex, nextDot)
        }
        val dfDelta = Map(nextDot -> LastWriterWins.now(e))

        ReplicatedList(
          order = glistDelta,
          elements = dfDelta,
        )
    }
  }

  def insertAll(i: Int, elems: Iterable[E])(using LocalUid): ReplicatedList[E] = {
    val nextDot = observed.nextDot(LocalUid.replicaId)

    val nextDots = List.iterate(nextDot, elems.size) {
      case Dot(c, r) => Dot(c, r + 1)
    }

    findInsertIndex(i) match {
      case None                   => ReplicatedList.empty
      case Some(glistInsertIndex) =>
        val glistDelta =
          order.map { gl =>
            gl.insertAllAt(glistInsertIndex, nextDots)
          }
        val dfDelta = Map.empty[Dot, LastWriterWins[E]] ++ (nextDots zip elems.map(e => LastWriterWins.now(e)))

        ReplicatedList(
          order = glistDelta,
          elements = dfDelta,
        )
    }
  }

  private def updateRGANode(state: ReplicatedList[E], i: Int, newNode: Option[E]): ReplicatedList[E] = {
    state.order.value.toLazyList.lift(i) match {
      case None    => ReplicatedList.empty
      case Some(d) =>
        state.elements.get(d) match
          case None          => ReplicatedList.empty
          case Some(current) =>
            newNode match
              case None        => ReplicatedList(deleted = Dots.single(d))
              case Some(value) =>
                ReplicatedList(elements = Map(d -> current.write(value)))
    }
  }

  def update(i: Int, e: E): ReplicatedList[E] = setAtIndex(i, Some(e))

  def delete(i: Int): ReplicatedList[E] = setAtIndex(i, None)

  def findUpdateIndex(n: Int): Option[Int] = {
    order.value.toLazyList.zip(LazyList.from(0)).filter {
      case (dot, _) => elements.contains(dot)
    }.map(_._2).lift(n)
  }

  def setAtIndex(i: Int, e: Option[E]): ReplicatedList[E] = {
    findUpdateIndex(i) match {
      case Some(index) => updateRGANode(this, index, e)
      case None        => ReplicatedList.empty[E]
    }
  }

  private def updateRGANodeBy(
      state: ReplicatedList[E],
      cond: E => Boolean,
      transform: LastWriterWins[E] => Option[LastWriterWins[E]]
  ): ReplicatedList[E] = {
    val touched: Iterable[Dot] = state.elements.flatMap: (k, v) =>
      Option.when(cond(v.payload))(k)

    val updates =
      touched.flatMap: dot =>
        val value = state.elements(dot)
        transform(value).map(nv => dot -> nv)
      .toMap

    ReplicatedList(elements = updates, deleted = Dots.from(touched).subtract(Dots.from(updates.keys)))
  }

  def updateBy(cond: E => Boolean, e: E): ReplicatedList[E] =
    updateRGANodeBy(this, cond, old => Some(old.write(e)))

  def deleteBy(cond: E => Boolean): ReplicatedList[E] =
    updateRGANodeBy(this, cond, _ => None)

  /** Note: this operation may drop concurrent additions to removed items. */
  def purgeTombstones(): ReplicatedList[E] = {
    val known: List[Dot] = order.value.toList

    val removed = known.filter(dot => !observed.contains(dot))

    val golistPurged = order.value.without(removed.toSet)

    ReplicatedList(
      order = order.epocheWrite(golistPurged),
    )
  }

  def clear(): ReplicatedList[E] = {
    ReplicatedList(
      deleted = Dots.from(elements.keys)
    )
  }

  def prepend(using LocalUid)(e: E): ReplicatedList[E] = insert(0, e)

  def append(using LocalUid)(e: E): ReplicatedList[E] = insert(sizeIncludingDeadElements, e)

  def prependAll(using LocalUid)(elems: Iterable[E]): ReplicatedList[E] = insertAll(0, elems)

  def appendAll(using LocalUid)(elems: Iterable[E]): ReplicatedList[E] = insertAll(sizeIncludingDeadElements, elems)

}
object ReplicatedList {

  def empty[E]: ReplicatedList[E] = ReplicatedList(Epoch.empty, Map.empty, Dots.empty)

  given lattice[E]: Lattice[ReplicatedList[E]] =
    DecoratedLattice.filter(Lattice.derived[ReplicatedList[E]]) { (base, other) =>
      val elem = base.elements.removedAll(other.deleted.iterator)
      base.copy(elements = elem)
    }

  given decompose[E]: Decompose[ReplicatedList[E]] = Decompose.derived

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty

}
