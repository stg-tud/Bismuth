package riblt

import riblt.Operation.{Add, Remove}

/**
 * a CodingWindow is a collection of a source Symbols, that can produce the next coded Symbol based on the these
 * source symbols
 * @param symbols
 * @param nextIndex
 * @tparam T
 */
class CodingWindow[T](
                       var symbols: List[SourceSymbol[T]] = List.empty[SourceSymbol[T]],
                       var nextIndex: Int = 0
):

  def addSourceSymbol(symbol: T)(using Hashable[T]): Unit =
    addSourceSymbol(SourceSymbol(symbol))

  def addSourceSymbol(sourceSymbol: SourceSymbol[T]): Unit =
    symbols = symbols :+ sourceSymbol

  def addSourceSymbolWithMapping(sourceSymbol: SourceSymbol[T], mapping: Mapping): Unit =
    val result = sourceSymbol
    result.mapping = mapping

    symbols = symbols :+ result

  def produceNextCodedSymbol(using Xorable[T]): CodedSymbol[T] =
    assert(symbols.nonEmpty, "you have to add source symbols first")
    applyCodedSymbol(CodedSymbol.identity, Add)

  def applyCodedSymbol(codedSymbol: CodedSymbol[T], op: Operation)(using Xorable[T]): CodedSymbol[T] =
    var result = codedSymbol

    for element <- symbols do
      if element.mapping.lastIndex == nextIndex then
        result = op match
          case Add    => result.add(element)
          case Remove => result.remove(element)

        element.mapping.nextIndex: Unit
    
    nextIndex = nextIndex + 1
    result


enum Operation:
  case Add
  case Remove
