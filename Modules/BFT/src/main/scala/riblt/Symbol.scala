package riblt

class HashedSymbol[T](
    var symbol: T,
    var hash: Long
)

object HashedSymbol:
  def identity[T](using Xorable[T])(using t: T): HashedSymbol[T] =
    HashedSymbol(t.zero, 0L)


class CodedSymbol[T](
    var hashedSymbol: HashedSymbol[T],
    var count: Long
):
  def add(sourceSymbol: HashedSymbol[T], direction: Int = 1)(using Xorable[T]): CodedSymbol[T] =
    hashedSymbol.symbol = hashedSymbol.symbol.xor(sourceSymbol.symbol)
    hashedSymbol.hash ^= sourceSymbol.hash
    count += direction
    this

object CodedSymbol:
  def identity[T](using Xorable[T])(using T): CodedSymbol[T] =
    CodedSymbol(HashedSymbol.identity, 0)