package riblt

trait Hashable[A]:
  extension (a: A) def hash: Long
