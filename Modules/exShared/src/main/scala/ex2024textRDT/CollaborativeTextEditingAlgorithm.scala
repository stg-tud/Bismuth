package ex2024textRDT

trait CollaborativeTextEditingAlgorithm[A] {

  extension (algorithm: A) {
    def insert(i: Int, x: Char): Unit

    def delete(i: Int): Unit

    def text(): String

    def sync(other: A): Unit

    def syncFrom(other: A): Unit
  }
}
