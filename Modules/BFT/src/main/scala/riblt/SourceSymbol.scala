package riblt

class SourceSymbol[T](
    var value: T,
    var hash: Long,
    var mapping: Mapping
)

object SourceSymbol:
  def identity[T](using x: Xorable[T])(using Hashable[T]): SourceSymbol[T] =
    SourceSymbol.apply(x.zero)

  def apply[T](symbol: T)(using Hashable[T]): SourceSymbol[T] =
    new SourceSymbol[T](symbol, symbol.hash, new Mapping(symbol.hash))
