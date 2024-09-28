package text_rdt

final case class StringEditory() extends Editory {
  var data: StringBuilder = StringBuilder()

  override def insert(index: Int, element: Char): Unit = {
    data.insert(index, element)
  }

  override def delete(index: Int): Unit = {
    data.deleteCharAt(index)
  }

  override def localInsert(index: Int, element: Char): Unit = {
    data.insert(index, element)
  }

  override def localDelete(index: Int): Unit = {
    data.deleteCharAt(index)
  }
}
