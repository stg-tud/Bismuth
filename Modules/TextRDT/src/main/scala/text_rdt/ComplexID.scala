package text_rdt

import scala.math.Ordered.orderingToOrdered

final case class ComplexID(rid: RID, counter: Int, offset: Int) {

  def toSimpleID: SimpleID | Null = {
    SimpleID(rid, counter)
  }
}

given complexId: Idy[ComplexID] with {

  override def compare(x: ComplexID, y: ComplexID): Int =
    (x.rid, x.counter, x.offset).compare((y.rid, y.counter, y.offset))
}
