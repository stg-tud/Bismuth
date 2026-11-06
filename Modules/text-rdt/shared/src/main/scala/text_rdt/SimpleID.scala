package text_rdt

final case class SimpleID(rid: RID, counter: Int) {}

given simpleId: Idy[SimpleID] with {

  override def compare(x: SimpleID, y: SimpleID): Int = {
    val cmp = x.rid.compareTo(y.rid)
    if (cmp == 0) { y.counter - x.counter }
    else { cmp }
  }
}
