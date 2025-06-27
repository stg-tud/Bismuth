package rdts.datatypes

import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.time.CausalTime

import scala.collection.mutable.ArrayBuffer

case class GrowOnlyList[E](order: Map[CausalTime, Set[CausalTime]], elements: Map[CausalTime, E]) {
  lazy val now: Option[CausalTime] = order.values.flatten.reduceOption(Lattice.merge)
  def nextTime: CausalTime         = now.map(_.advance).getOrElse(CausalTime.now())

  def insertAfter(predecessor: CausalTime, value: E) = {
    val next = nextTime
    GrowOnlyList(order = Map(predecessor -> Set(next)), elements = Map(next -> value))
  }

  def toposort(): Seq[CausalTime] = {
    val sorted                      = ArrayBuffer[CausalTime]()
    var discovered: Set[CausalTime] = Set.empty

    def _toposort(rem: CausalTime): Unit = {
      if discovered.contains(rem) then ()
      else {
        discovered = discovered + rem
        val next = order.get(rem).iterator.flatten.toSeq.sorted
        next.foreach(_toposort)
        sorted += rem
        ()
      }
    }

    (Iterable(GrowOnlyList.headDot) ++ order.keys.toSeq.sorted).foreach(_toposort)
    sorted.toSeq.reverse
  }

  def dotList: Seq[CausalTime] = toposort()

  def toList: List[E] = dotList.flatMap(elements.get).toList

  def toLazyList: LazyList[E] = toList.to(LazyList)

  def read(i: Int): Option[E] = toList.lift(i)

  def insertGL(index: Int, elem: E): GrowOnlyList[E] = {
    val pos = dotList(index)
    insertAfter(pos, elem)
  }
  def insertAllGL(index: Int, elems: List[E]): GrowOnlyList[E] = {
    val pos = dotList(index)
    val res = elems.scanLeft(this)((gl, d) => gl.insertAfter(pos, d))
    res.drop(1).reduceOption(_.merge(_)).getOrElse(GrowOnlyList.empty)
  }

  def size: Int = elements.size

  def without(elems: Set[E]): GrowOnlyList[E] = GrowOnlyList(order, elements.filter((_, e) => !elems.contains(e)))
}

object GrowOnlyList {
  val headDot = CausalTime.empty

  given decompose[E]: Decompose[GrowOnlyList[E]] = {
    given Decompose[E] = Decompose.atomic
    Decompose.derived
  }

  given bottom[E]: Bottom[GrowOnlyList[E]]   = Bottom.derived
  def empty[E]: GrowOnlyList[E]              = bottom.empty
  given lattice[E]: Lattice[GrowOnlyList[E]] = {
    given Lattice[E] = Lattice.assertEquals

    Lattice.derived
  }

}
