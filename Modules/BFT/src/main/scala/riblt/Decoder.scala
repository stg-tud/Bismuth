package riblt

given ord: Ordering[SymbolMapping] = Ordering.by(_.codedIndex)

class Decoder[T](
    var codedSymbol: List[CodedSymbol[T]],
    var local: CodingWindow[T],
    var window: CodingWindow[T],
    var remote: CodingWindow[T],
    var decodable: List[Int],
    var decoded: Int
):
  
  def isDecoded: Boolean =
    decoded == codedSymbol.length

  def localSymbols: List[HashedSymbol[T]] =
    local.symbols

  def remoteSymbols: List[HashedSymbol[T]] =
    remote.symbols

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    addHashedSymbol(HashedSymbol(symbol, symbol.hash))

  def addHashedSymbol(hashedSymbol: HashedSymbol[T]): Unit =
    window.addHashedSymbol(hashedSymbol)

  def addCodedSymbol(codedSymbol: CodedSymbol[T]): Unit =
    ???
