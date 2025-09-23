package riblt

class CodedSymbol[T](
                      var sum: T,
                      var hash: Long,
                      var count: Long
                    ):

  def add(sourceSymbol: SourceSymbol[T])(using Xorable[T]): CodedSymbol[T] =
    apply(sourceSymbol)
    count += 1
    this

  def remove(sourceSymbol: SourceSymbol[T])(using Xorable[T]): CodedSymbol[T] =
    apply(sourceSymbol)
    count -= 1
    this

  def apply(sourceSymbol: SourceSymbol[T])(using Xorable[T]): Unit =
    sum = sum.xor(sourceSymbol.symbol)
    hash ^= sourceSymbol.hash

object CodedSymbol:
  def identity[T](using x: Xorable[T]): CodedSymbol[T] =
    CodedSymbol(x.zero, 0L, 0L)