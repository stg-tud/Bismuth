package riblt

class Encoder[T](
    var window: CodingWindow[T] = CodingWindow[T]()
):

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    window.addSymbol(symbol)

  def produceNextCodedSymbol(using t: T)(using Xorable[T]): CodedSymbol[T] =
    window.produceNextCodedSymbol
