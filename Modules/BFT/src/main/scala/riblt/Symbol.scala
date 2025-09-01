package riblt

class HashedSymbol[T] (
                        var symbol: T,
                        var hash: BigInt
                      )

class CodedSymbol[T] (
                       var hashedSymbol: HashedSymbol[T],
                       var count: BigInt
                     ):
  def add(sourceSymbol: HashedSymbol[T], direction: Int = 1)(using Xorable[T]): CodedSymbol[T] =
    hashedSymbol.symbol = hashedSymbol.symbol.xor(sourceSymbol.symbol)
    hashedSymbol.hash ^= sourceSymbol.hash
    count += direction
    this


