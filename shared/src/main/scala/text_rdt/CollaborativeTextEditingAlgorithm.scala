package text_rdt

trait CollaborativeTextEditingAlgorithm {
  def insert(i: Int, x: Char): Unit

  def delete(i: Int): Unit
}
