package riblt

class SourceSymbol[T](
    var symbol: T,
    var hash: Long
)

object SourceSymbol:
  def identity[T](using x: Xorable[T]): SourceSymbol[T] =
    SourceSymbol(x.zero, 0L)
