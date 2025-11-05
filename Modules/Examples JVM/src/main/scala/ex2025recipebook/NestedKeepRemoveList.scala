package ex2025recipebook

import rdts.base.*
import rdts.base.Historized.MetaDelta
import rdts.datatypes.{EnableWinsFlag, Epoch, GrowOnlyList}
import rdts.time.{Dot, Dots}

/** Keep remove list with an arbitrary crdt instead of lww as payload
  * @param order
  * @param payloads
  * @param flags
  * @tparam E
  */
case class NestedKeepRemoveList[E] private (
    order: Epoch[GrowOnlyList[Dot]] = empty.order,
    payloads: Map[Dot, E] = Map.empty,
    flags: Map[Dot, EnableWinsFlag] = Map.empty
) {
  private type C = NestedKeepRemoveList[E]

  def size: Int = order.value.toLazyList.count(d => isAlive(d))

  def read(idx: Int): Option[E] =
    order.value.toLazyList
      .filter(isAlive)
      .flatMap(d => payloads.get(d))
      .lift(idx)

  def read(dot: Dot): Option[E] = if isAlive(dot) then payloads.get(dot) else None

  def toList: List[E] =
    order.value.toList.flatMap { d => if isAlive(d) then payloads.get(d) else None }

  def insertAt(i: Int, e: E)(using LocalUid): C = {
    val newDot = observed.nextDot(LocalUid.replicaId)
    findInsertIndex(i) match
        case None        => NestedKeepRemoveList.empty
        case Some(glIdx) =>
          val nOrder   = order.map(_.insertAt(glIdx, newDot))
          val nPayload = Map(newDot -> e)
          val nFlag    = Map(newDot -> EnableWinsFlag(Dots.single(newDot), Dots.empty))
          NestedKeepRemoveList(order = nOrder, payloads = nPayload, flags = nFlag)
  }

  def append(using LocalUid)(e: E): C = insertAt(sizeIncludingDead, e)

  def appendAll(using LocalUid)(es: Iterable[E]): C = es.foldLeft(this)((list, e) => list.append(e))

  def keep(idx: Int)(using LocalUid): C =
    updateFlag(idx) { case flag =>
      flag.enable()
    }

  def update(idx: Int, mod: (E) => E)(using LocalUid): C = {
    read(idx) match {
      case Some(value) =>
        findRealIndex(idx) match
            case None          => NestedKeepRemoveList.empty
            case Some(realIdx) =>
              order.value.toLazyList.lift(realIdx) match
                  case None    => NestedKeepRemoveList.empty
                  case Some(d) =>
                    val cur  = flags.getOrElse(d, EnableWinsFlag.empty)
                    val next = cur.enable()
                    if cur == next then NestedKeepRemoveList.empty
                    else NestedKeepRemoveList(flags = Map(d -> next), payloads = Map(d -> mod(value)))
      case None => NestedKeepRemoveList.empty
    }
  }

  def remove(idx: Int)(using LocalUid): C =
    updateFlag(idx) { case flag =>
      flag.disable()
    }

  def purgeTombstones(): C =
      val dead = flags.collect { case (d, f) if !f.read => d }.toSet
      if dead.isEmpty then NestedKeepRemoveList.empty
      else
          val nOrder    = order.map(_.without(dead))
          val nPayloads = payloads -- dead
          val nFlags    = flags -- dead
          NestedKeepRemoveList(order = nOrder, payloads = nPayloads, flags = nFlags)

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

  private def updateFlag(idx: Int)(f: (EnableWinsFlag) => EnableWinsFlag)(using LocalUid): C =
    findRealIndex(idx) match
        case None          => NestedKeepRemoveList.empty
        case Some(realIdx) =>
          order.value.toLazyList.lift(realIdx) match
              case None    => NestedKeepRemoveList.empty
              case Some(d) =>
                val cur  = flags.getOrElse(d, EnableWinsFlag.empty)
                val next = f(cur)
                if cur == next then NestedKeepRemoveList.empty else NestedKeepRemoveList(flags = Map(d -> next))
}

object NestedKeepRemoveList {
  def empty[E]: NestedKeepRemoveList[E] = NestedKeepRemoveList(Epoch.empty, Map.empty, Map.empty)

  given bottom[E]: Bottom[NestedKeepRemoveList[E]] with
      def empty: NestedKeepRemoveList[E] = NestedKeepRemoveList.empty

  given lattice[E: Lattice]: Lattice[NestedKeepRemoveList[E]] = Lattice.derived

//  given historized[E: Historized]: Historized[NestedKeepRemoveList[E]] =
//    (delta: NestedKeepRemoveList[E], buffer: Iterable[MetaDelta[NestedKeepRemoveList[E]]]) => {
//      println(s"get redundant dots for KR-List:\n\tdelta: $delta\n\tbuffer: ${buffer.size} $buffer")
//      val redundant = delta match {
//        case NestedKeepRemoveList(order, payloads, flags) if order.isEmpty && payloads.isEmpty && flags.nonEmpty =>
//          // keep or remove operations overrides concurrent removes or previous keep operations
//          buffer.filter(bufferedDelta =>
//            flags.forall((dot, f) =>
//              bufferedDelta.delta.flags.contains(dot) &&
//              ((f.read && !bufferedDelta.delta.flags(dot).read) || (!f.read && bufferedDelta.delta.flags(dot).read))
//            )
//          ).getAllDots
//        case NestedKeepRemoveList(order, payloads, flags) if order.isEmpty && payloads.nonEmpty && flags.nonEmpty =>
//          // update operation
//          buffer.filter(bufferedDelta => flags.forall((dot, _) => bufferedDelta.delta.flags.contains(dot)))
//            .foldLeft(Dots.empty)((dots, bufferedDelta) =>
//              dots.union(bufferedDelta.delta.payloads.foldLeft(Dots.empty)((dots, entry) =>
//                dots.union(delta.payloads(entry._1).getRedundantDeltas(
//                  buffer.filter(bufferedDelta => bufferedDelta.delta.payloads.contains(entry._1))
//                    .mapDeltas(_.payloads(entry._1))
//                ))
//              ))
//            )
//        case NestedKeepRemoveList(order, payloads, flags)
//            if !order.isEmpty && payloads.nonEmpty && flags.nonEmpty && flags.forall(_._2.read) =>
//          // insert operation overwrites concurrent remove and keep operations
//          buffer.filter(bufferedDelta => flags.forall((dot, _) => bufferedDelta.delta.flags.contains(dot))).getAllDots
//        case _ => Dots.empty
//      }
//
//      println(f"\tredundant: $redundant")
//      redundant
//    }
}
