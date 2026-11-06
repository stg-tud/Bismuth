package text_rdt

trait Rdty {
  def onInsert(index: Int, character: Char): Unit

  def onDelete(index: Int): Unit

}
