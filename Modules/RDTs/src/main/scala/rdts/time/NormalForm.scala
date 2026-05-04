package rdts.time

trait NormalForm[T]:
    extension (tree: T) def normalized: T
