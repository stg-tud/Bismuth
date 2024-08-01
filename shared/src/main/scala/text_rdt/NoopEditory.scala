package text_rdt

final case class NoopEditory() extends Editory {

  override def insert(index: Int, element: Char): Unit = {}

  override def delete(index: Int): Unit = {}

}
