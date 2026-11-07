package ex2024textRDT

trait Editory {
  def insert(index: Int, element: Char): Unit

  def delete(index: Int): Unit

  def localInsert(index: Int, element: Char): Unit = {}

  def localDelete(index: Int): Unit = {}
}
