package rdts.datatypes

import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import ReplicatedList.deltaState
import rdts.datatypes.{Epoch, GrowOnlyList, LastWriterWins}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{Dotted, HasDots}
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
case class ReplicatedList[E](order: Epoch[GrowOnlyList[Dot]], meta: Dotted[Map[Dot, LastWriterWins[E]]]) {

  private def current = this
  type C = ReplicatedList[E]

  def read(i: Int): Option[E] = {
    val ReplicatedList(fw, df) = this
    fw.value.toLazyList.flatMap(df.data.get).map(_.payload).lift(i)
  }

  def size: Int = meta.data.size

  def toList: List[E] = {
    val ReplicatedList(fw, df) = this
    fw.value.toList.flatMap(df.data.get).map(_.payload)
  }

  def sequence: Long = {
    val ReplicatedList(fw, _) = this
    fw.counter
  }

  def findInsertIndex(n: Int): Option[Int] = {
    current.order.value.toLazyList.zip(LazyList.from(1)).filter {
      case (dot, _) => current.meta.data.contains(dot)
    }.map(_._2).prepended(0).lift(n)
  }

  /** Inserts at list index `i` */
  def insert(using LocalUid)(i: Int, e: E): C = {
    val ReplicatedList(order, entries) = current
    val nextDot                        = meta.context.nextDot(LocalUid.replicaId)

    findInsertIndex(i) match {
      case None                   => ReplicatedList.empty[E]
      case Some(glistInsertIndex) =>
        val glistDelta = order.map { gl =>
          gl.insertGL(glistInsertIndex, nextDot)
        }
        val dfDelta = Map(nextDot -> LastWriterWins.now(e))

        deltaState[E].make(
          epoche = glistDelta,
          df = dfDelta,
          cc = Dots.single(nextDot)
        )
    }
  }

  def insertAll(using LocalUid)(i: Int, elems: Iterable[E]): C = {
    val ReplicatedList(fw, df) = current
    val nextDot                = meta.context.nextDot(LocalUid.replicaId)

    val nextDots = List.iterate(nextDot, elems.size) {
      case Dot(c, r) => Dot(c, r + 1)
    }

    findInsertIndex(i) match {
      case None                   => ReplicatedList.empty
      case Some(glistInsertIndex) =>
        val glistDelta =
          fw.map { gl =>
            gl.insertAllGL(glistInsertIndex, nextDots)
          }
        val dfDelta = Map.empty[Dot, LastWriterWins[E]] ++ (nextDots zip elems.map(e => LastWriterWins.now(e)))

        deltaState[E].make(
          epoche = glistDelta,
          df = dfDelta,
          cc = Dots.from(nextDots.toSet)
        )
    }
  }

  private def updateRGANode(state: ReplicatedList[E], i: Int, newNode: Option[E]): ReplicatedList[E] = {
    val ReplicatedList(fw, df) = state
    fw.value.toLazyList.lift(i) match {
      case None    => ReplicatedList.empty
      case Some(d) =>
        df.data.get(d) match
          case None          => ReplicatedList.empty
          case Some(current) =>
            newNode match
              case None        => deltaState[E].make(cc = Dots.single(d))
              case Some(value) =>
                deltaState[E].make(df = Map(d -> current.write(value)), cc = Dots.single(d))
    }
  }

  def update(using LocalUid)(i: Int, e: E): C = setAtIndex(i, Some(e))

  def delete(using LocalUid)(i: Int): C = setAtIndex(i, None)

  def findUpdateIndex(n: Int): Option[Int] = {
    current.order.value.toLazyList.zip(LazyList.from(0)).filter {
      case (dot, _) => current.meta.data.contains(dot)
    }.map(_._2).lift(n)
  }

  def setAtIndex(using LocalUid)(i: Int, e: Option[E]): C = {
    findUpdateIndex(i) match {
      case Some(index) => updateRGANode(current, index, e)
      case None        => ReplicatedList.empty[E]
    }
  }

  private def updateRGANodeBy(
      state: ReplicatedList[E],
      cond: E => Boolean,
      transform: LastWriterWins[E] => Option[LastWriterWins[E]]
  ): ReplicatedList[E] = {
    val touched: Iterable[Dot] = state.meta.data.flatMap: (k, v) =>
      Option.when(cond(v.payload))(k)

    val updates =
      touched.flatMap: dot =>
        val value = state.meta.data(dot)
        transform(value).map(nv => dot -> nv)
      .toMap

    deltaState[E].make(df = updates, cc = Dots.from(touched))
  }

  def updateBy(using LocalUid)(cond: E => Boolean, e: E): C =
    updateRGANodeBy(current, cond, old => Some(old.write(e)))

  def deleteBy(using LocalUid)(cond: E => Boolean): C =
    updateRGANodeBy(current, cond, _ => None)

  def purgeTombstones(using LocalUid)(): C = {
    val ReplicatedList(epoche, df) = current

    val known: List[Dot] = epoche.value.toList

    val contained = df.data.dots

    val removed = known.filter(dot => !contained.contains(dot))

    val golistPurged = epoche.value.without(removed.toSet)

    deltaState[E].make(
      epoche = epoche.epocheWrite(golistPurged),
      cc = Dots.from(removed)
    )
  }

  def clear(): C = {
    deltaState[E].make(
      cc = meta.context
    )
  }

  def prepend(using LocalUid)(e: E): C = insert(0, e)

  def append(using LocalUid)(e: E): C = insert(size, e)

  def prependAll(using LocalUid)(elems: Iterable[E]): C = insertAll(0, elems)

  def appendAll(using LocalUid)(elems: Iterable[E]): C = insertAll(size, elems)

}
object ReplicatedList {

  def empty[E]: ReplicatedList[E] = ReplicatedList(Epoch.empty, Dotted.empty)

  given lattice[E]: Lattice[ReplicatedList[E]]     = Lattice.derived
  given decompose[E]: Decompose[ReplicatedList[E]] =
    given Decompose[LastWriterWins[E]] = Decompose.atomic
    Decompose.derived

  given bottom[E]: Bottom[ReplicatedList[E]] = new:
    override def empty: ReplicatedList[E] = ReplicatedList.empty

  private class DeltaStateFactory[E] {

    def make(
        epoche: Epoch[GrowOnlyList[Dot]] = empty._1,
        df: Map[Dot, LastWriterWins[E]] = Map.empty,
        cc: Dots = Dots.empty
    ): ReplicatedList[E] = ReplicatedList(epoche, Dotted(df, cc))
  }

  private def deltaState[E]: DeltaStateFactory[E] = new DeltaStateFactory[E]

}
