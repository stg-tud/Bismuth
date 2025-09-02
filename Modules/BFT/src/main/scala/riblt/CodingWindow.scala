package riblt

import scala.collection.mutable

class CodingWindow[T](
    var symbols: List[HashedSymbol[T]],
    var mappings: List[Mapping],
    var queue: mutable.PriorityQueue[SymbolMapping] = mutable.PriorityQueue()(using ord.reverse),
    var nextIndex: Int = 0
):
  
  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    addHashedSymbol(HashedSymbol(symbol, symbol.hash))

  def addHashedSymbol(hashedSymbol: HashedSymbol[T]): Unit =
    addHashedSymbolWithMapping(hashedSymbol, new Mapping(hashedSymbol.hash))

  def addHashedSymbolWithMapping(hashedSymbol: HashedSymbol[T], mapping: Mapping): Unit =
    symbols = symbols :+ hashedSymbol
    mappings = mappings :+ mapping
    queue.enqueue(SymbolMapping(symbols.length - 1, mapping.lastIndex.toInt))
