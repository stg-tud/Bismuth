package riblt

import riblt.Operation.{Add, Remove}

class RIBLT[T](
    var codedSymbols: List[CodedSymbol[T]] = List.empty[CodedSymbol[T]],
    var local: CodingWindow[T] = new CodingWindow[T](),
    var window: CodingWindow[T] = new CodingWindow[T](),
    var remote: CodingWindow[T] = new CodingWindow[T](),
    var decodable: List[CodedSymbol[T]] = List.empty[CodedSymbol[T]],
):

  def isDecoded: Boolean =
    codedSymbols.forall(c => c.decoded)

  def localSymbols: List[SourceSymbol[T]] =
    local.symbols

  def remoteSymbols: List[SourceSymbol[T]] =
    remote.symbols

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    window.addSourceSymbol(symbol)

  def produceNextCodedSymbol(using Xorable[T]): CodedSymbol[T] =
    window.produceNextCodedSymbol

  def addCodedSymbol(codedSymbol: CodedSymbol[T])(using Xorable[T])(using Hashable[T]): Unit =
    var c = window.applyCodedSymbol(codedSymbol, Remove)
    c = remote.applyCodedSymbol(c, Remove)
    c = local.applyCodedSymbol(c, Add)

    codedSymbols = codedSymbols :+ c

    if c.count == 1 || c.count == -1 then
      c.sum = c.sum.removeTrailingZeros()

    if (c.count == 1 || c.count == -1) && (c.hash == c.sum.hash) then
      decodable = decodable :+ c
    else if c.count == 0 && c.hash == 0 then
      decodable = decodable :+ c

  def applyNewSymbol(sourceSymbol: SourceSymbol[T], op: Operation)(using Hashable[T])(using Xorable[T]): SourceSymbol[T] =
    var i = sourceSymbol.mapping.lastIndex.toInt
    while i < codedSymbols.length do
      val tmp = op match
        case Add => codedSymbols(i).add(sourceSymbol)
        case Remove => codedSymbols(i).remove(sourceSymbol)

      codedSymbols = codedSymbols.updated(i, tmp)

      if codedSymbols(i).count == -1 || codedSymbols(i).count == 1 then
        codedSymbols(i).sum = codedSymbols(i).sum.removeTrailingZeros()
        if codedSymbols(i).hash == codedSymbols(i).sum.hash then
          decodable = decodable :+ codedSymbols(i)

      i = sourceSymbol.mapping.nextIndex.toInt

    sourceSymbol

  def tryDecode(using Hashable[T])(using Xorable[T]): Unit =
    var i = 0
    while i < decodable.length do
      val c = decodable(i)

      c.count match
        case 1 =>
          val newSymbol = applyNewSymbol(SourceSymbol[T](c.sum), Remove)
          remote.addSourceSymbol(newSymbol)
          c.decoded = true
        case -1 =>
          val newSymbol = applyNewSymbol(SourceSymbol[T](c.sum), Add)
          local.addSourceSymbol(newSymbol)
          c.decoded = true
        case 0 =>
          c.decoded = true
        case _ =>
          throw new Exception("Invalid degree of decodable coded Symbol")

      i += 1

    decodable = List.empty[CodedSymbol[T]]


  def restart(): Unit = ???
