package riblt

import riblt.Operation.{Add, Remove}

import scala.collection.mutable

given ord: Ordering[SymbolMapping] = Ordering.by(_.codedIndex)

class CodingWindow[T](
    var symbols: List[HashedSymbol[T]] = List.empty[HashedSymbol[T]],
    var mappings: List[Mapping] = List.empty[Mapping],
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

  def produceNextCodedSymbol(using Xorable[T]): CodedSymbol[T] =
    assert(queue.nonEmpty, "you have to add source symbols first")
    applyCodedSymbol(CodedSymbol.identity, Add)

  def applyCodedSymbol(codedSymbol: CodedSymbol[T], op: Operation)(using Xorable[T]): CodedSymbol[T] =
    if queue.isEmpty then {
      nextIndex += 1
      codedSymbol
    } else {
      var tmp = codedSymbol

      val list = queue.toList
      for element <- list do {
        if element.codedIndex == nextIndex then {
          tmp = op match {
            case Add    => tmp.add(symbols(element.sourceIndex))
            case Remove => tmp.remove(symbols(element.sourceIndex))
          }
          element.codedIndex = mappings(element.sourceIndex).nextIndex.toInt
        }
      }

      nextIndex = nextIndex + 1
      tmp
    }

enum Operation:
  case Add
  case Remove
