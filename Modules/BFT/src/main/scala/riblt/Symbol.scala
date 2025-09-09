package riblt

class HashedSymbol[T](
    var symbol: T,
    var hash: Long
)

object HashedSymbol:
  def identity[T](using Xorable[T])(using t: T): HashedSymbol[T] =
    HashedSymbol(t.zero, 0L)

class CodedSymbol[T](
    var sum: T,
    var hash: Long,
    var count: Long
):

  def add(sourceSymbol: HashedSymbol[T])(using Xorable[T]): CodedSymbol[T] =
    apply(sourceSymbol)
    count += 1
    this

  def remove(sourceSymbol: HashedSymbol[T])(using Xorable[T]): CodedSymbol[T] =
    apply(sourceSymbol)
    count -= 1
    this

  def apply(sourceSymbol: HashedSymbol[T])(using Xorable[T]): Unit =
    sum = sum.xor(sourceSymbol.symbol)
    hash ^= sourceSymbol.hash

object CodedSymbol:
  def identity[T](using Xorable[T])(using t: T): CodedSymbol[T] =
    CodedSymbol(t.zero, 0L, 0L)
