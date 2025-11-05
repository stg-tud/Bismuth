package com.daimpl.lib

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{EnableWinsFlag, Epoch, GrowOnlyList, LastWriterWins}
import rdts.time.{Dot, Dots}

import scala.annotation.nowarn

/** KeepRemoveList — a list CRDT with **keep‑wins** semantics.
  *
  * A delete is effective *only* if the replica had already observed every keep
  * it is about to invalidate.  Technically this is realised with an
  * OR‑Flag (see below):
  *   • every **keep** adds a fresh dot to a `keeps` set
  *   • **remove** adds *all* currently seen keep‑dots into a `removed` set
  * The element is visible iff `keeps \ removed` is non‑empty.  Hence a
  * concurrent remove (that hasn’t seen a newer keep‑dot) cannot erase that
  * dot, while a causally‑after remove *can*.
  */
case class KeepRemoveList[E] private (
    order: Epoch[GrowOnlyList[Dot]] = empty.order,
    payloads: Map[Dot, LastWriterWins[E]] = Map.empty,
    flags: Map[Dot, EnableWinsFlag] = Map.empty: @nowarn("id=E198")
) {
  private type C = KeepRemoveList[E]

  def size: Int = order.value.toLazyList.count(d => isAlive(d))

  def read(idx: Int): Option[E] =
    order.value.toLazyList
      .filter(isAlive)
      .flatMap(d => payloads.get(d).map(_.payload))
      .lift(idx)

  def toList: List[E] =
    order.value.toList.flatMap { d => if isAlive(d) then payloads.get(d).map(_.payload) else None }

  def insertAt(i: Int, e: E)(using LocalUid): C = {
    val newDot = observed.nextDot(LocalUid.replicaId)
    findInsertIndex(i) match
       case None        => KeepRemoveList.empty
       case Some(glIdx) =>
         val nOrder   = order.map(_.insertAt(glIdx, newDot))
         val nPayload = Map(newDot -> LastWriterWins.now(e))
         val nFlag    = Map(newDot -> EnableWinsFlag(Dots.single(newDot), Dots.empty))
         KeepRemoveList(order = nOrder, payloads = nPayload, flags = nFlag)
  }

  def append(using LocalUid)(e: E): C = insertAt(sizeIncludingDead, e)

  def keep(idx: Int)(using LocalUid): C =
    updateFlag(idx) { case flag =>
      flag.enable()
    }

  def remove(idx: Int): C =
    updateFlag(idx) { case flag =>
      flag.disable()
    }

  def purgeTombstones(): C =
     val dead = flags.collect { case (d, f) if !f.read => d }.toSet
     if dead.isEmpty then KeepRemoveList.empty
     else
        val nOrder    = order.map(_.without(dead))
        val nPayloads = payloads -- dead
        val nFlags    = flags -- dead
        KeepRemoveList(order = nOrder, payloads = nPayloads, flags = nFlags)

  private def isAlive(d: Dot): Boolean = flags.get(d).forall(_.read)

  private def sizeIncludingDead: Int = payloads.size

  private def observed: Dots =
    Dots.from(payloads.keys).union(flags.values.foldLeft(Dots.empty)((s, f) => s.union(f.set).union(f.unset)))

  private def findInsertIndex(n: Int): Option[Int] =
    order.value.toLazyList.zip(LazyList.from(1))
      .filter((d, _) => payloads.contains(d))
      .map(_._2).prepended(0).lift(n)

  private def findRealIndex(n: Int): Option[Int] =
    order.value.toLazyList.zip(LazyList.from(0))
      .filter((d, _) => isAlive(d))
      .map(_._2).lift(n)

  private def updateFlag(idx: Int)(f: EnableWinsFlag => EnableWinsFlag): C =
    findRealIndex(idx) match
       case None          => KeepRemoveList.empty
       case Some(realIdx) =>
         order.value.toLazyList.lift(realIdx) match
            case None    => KeepRemoveList.empty
            case Some(d) =>
              val cur  = flags.getOrElse(d, EnableWinsFlag.empty)
              val next = f(cur)
              if cur == next then KeepRemoveList.empty else KeepRemoveList(flags = Map(d -> next))
}

object KeepRemoveList {
  def empty[E]: KeepRemoveList[E] = KeepRemoveList(Epoch.empty, Map.empty, Map.empty)

  given bottom[E]: Bottom[KeepRemoveList[E]] with
     def empty: KeepRemoveList[E] = KeepRemoveList.empty

  given lattice[E]: Lattice[KeepRemoveList[E]] = Lattice.derived
}
