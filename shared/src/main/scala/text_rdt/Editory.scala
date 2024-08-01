package text_rdt

trait Editory {
  def insert(index: Int, element: Char): Unit

  def delete(index: Int): Unit
}
