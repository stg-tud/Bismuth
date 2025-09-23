package riblt

import riblt.Operation.{Add, Remove}

import scala.collection.mutable

given ord: Ordering[SymbolMapping] = Ordering.by(_.codedIndex)

class CodingWindow[T](
                       var symbols: List[SourceSymbol[T]] = List.empty[SourceSymbol[T]],
                       var mappings: List[Mapping] = List.empty[Mapping],
                       var queue: mutable.PriorityQueue[SymbolMapping] = mutable.PriorityQueue()(using ord.reverse),
                       var nextIndex: Int = 0
):

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    addSourceSymbol(SourceSymbol(symbol, symbol.hash))

  def addSourceSymbol(sourceSymbol: SourceSymbol[T]): Unit =
    addSourceSymbolWithMapping(sourceSymbol, new Mapping(sourceSymbol.hash))

  def addSourceSymbolWithMapping(sourceSymbol: SourceSymbol[T], mapping: Mapping): Unit =
    symbols = symbols :+ sourceSymbol
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
