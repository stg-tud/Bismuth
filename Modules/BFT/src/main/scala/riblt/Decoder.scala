package riblt

import riblt.Operation.{Add, Remove}

class Decoder[T](
    var codedSymbols: List[CodedSymbol[T]] = List.empty[CodedSymbol[T]],
    var local: CodingWindow[T] = new CodingWindow[T](),
    var window: CodingWindow[T] = new CodingWindow[T](),
    var remote: CodingWindow[T] = new CodingWindow[T](),
    var decodable: List[Int] = List.empty[Int],
    var decoded: Int = 0
):

  def isDecoded: Boolean =
    decoded == codedSymbols.length

  def localSymbols: List[HashedSymbol[T]] =
    local.symbols

  def remoteSymbols: List[HashedSymbol[T]] =
    remote.symbols

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    window.addSymbol(symbol)

  def addCodedSymbol(codedSymbol: CodedSymbol[T])(using Xorable[T])(using Hashable[T]): Unit =
    var c = window.applyCodedSymbol(codedSymbol, Remove)
    c = remote.applyCodedSymbol(c, Remove)
    c = local.applyCodedSymbol(c, Add)

    codedSymbols = codedSymbols :+ c

    if (c.count == 1 || c.count == -1) && (c.hash == c.sum.hash) then
      decodable = decodable :+ (codedSymbols.length - 1)
    else if c.count == 0 && c.hash == 0 then
      decodable = decodable :+ (codedSymbols.length - 1)

  def applyNewSymbol(hashedSymbol: HashedSymbol[T], op: Operation)(using Hashable[T])(using Xorable[T]): Mapping =
    val m = new Mapping(hashedSymbol.hash)

    while m.lastIndex.toInt < codedSymbols.length do
      val i   = m.lastIndex.toInt
      val tmp = op match
        case Add    => codedSymbols(i).add(hashedSymbol)
        case Remove => codedSymbols(i).remove(hashedSymbol)

      codedSymbols = codedSymbols.updated(i, tmp)

      if (codedSymbols(i).count == -1 || codedSymbols(i).count == 1) && codedSymbols(i).hash == codedSymbols(i).sum.hash
      then
        decodable = decodable :+ i

      val q = m.nextIndex

    m

  def tryDecode(using Hashable[T])(using Xorable[T]): Unit = {
    var i = 0
    while i < decodable.length do {
      val codedIndex = decodable(i)
      val c          = codedSymbols(codedIndex)

      c.count match
        case 1 =>
          val newSymbol = HashedSymbol[T](c.sum, c.hash)
          val m         = applyNewSymbol(newSymbol, Remove)
          remote.addHashedSymbolWithMapping(newSymbol, m)
          decoded += 1
        case -1 =>
          val newSymbol = HashedSymbol[T](c.sum, c.hash)
          val m         = applyNewSymbol(newSymbol, Add)
          local.addHashedSymbolWithMapping(newSymbol, m)
          decoded += 1
        case 0 =>
          decoded += 1
        case _ =>
          throw new Exception("Invalid degree of decodable coded Symbol")

      i += 1
    }

    decodable = List.empty[Int]

  }
