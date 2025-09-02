package riblt

import scala.collection.mutable
import java.security.MessageDigest

class CodingWindow[T](
    var symbols: List[HashedSymbol[T]],
    var mappings: List[Mapping],
    var queue: mutable.PriorityQueue[SymbolMapping] = mutable.PriorityQueue()(using ord.reverse),
    var nextIndex: Int = 0
):

  private val HashAlgorithm = "SHA3-512"

  def addSymbol(symbol: T): Unit =
    val hash = MessageDigest.getInstance(HashAlgorithm).digest(symbol.toString.getBytes)
    addHashedSymbol(HashedSymbol(symbol, BigInt(1, hash)))

  def addHashedSymbol(hashedSymbol: HashedSymbol[T]): Unit =
    addHashedSymbolWithMapping(hashedSymbol, new Mapping(hashedSymbol.hash))

  def addHashedSymbolWithMapping(hashedSymbol: HashedSymbol[T], mapping: Mapping): Unit =
    symbols = symbols :+ hashedSymbol
    mappings = mappings :+ mapping
    queue.enqueue(SymbolMapping(symbols.length - 1, mapping.lastIndex.toInt))
