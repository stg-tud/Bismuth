package riblt

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

given ord: Ordering[SymbolMapping] = Ordering.by(_.codedIndex)

class Encoder[T](
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

  def produceNextCodedSymbol(t: T)(using Xorable[T]): CodedSymbol[T] =
    assert(queue.nonEmpty, "you have to add source symbols first")

    var codedSymbol = CodedSymbol[T](HashedSymbol[T](t, 0), 0)

    queue.foreach(element => {
      if element.codedIndex == nextIndex then
        codedSymbol = codedSymbol.add(symbols(element.sourceIndex))
        element.codedIndex = mappings(element.sourceIndex).nextIndex.toInt
    })

    nextIndex = nextIndex + 1
    codedSymbol
