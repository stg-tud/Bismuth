package riblt

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import java.security.MessageDigest

given ord: Ordering[SymbolMapping] = Ordering.by(_.codedIndex)

class Decoder[T](
    var codedSymbol: List[CodedSymbol[T]],
    var local: CodingWindow[T],
    var window: CodingWindow[T],
    var remote: CodingWindow[T],
    var decodable: List[Int],
    var decoded: Int
):

  private val HashAlgorithm = "SHA3-512"

  def isDecoded: Boolean =
    decoded == codedSymbol.length

  def localSymbols: List[HashedSymbol[T]] =
    local.symbols

  def remoteSymbols: List[HashedSymbol[T]] =
    remote.symbols

  def addSymbol(symbol: T): Unit =
    val hash = MessageDigest.getInstance(HashAlgorithm).digest(symbol.toString.getBytes)
    addHashedSymbol(HashedSymbol(symbol, BigInt(1, hash)))

  def addHashedSymbol(hashedSymbol: HashedSymbol[T]): Unit =
    window.addHashedSymbol(hashedSymbol)

  def addCodedSymbol(codedSymbol: CodedSymbol[T]): Unit =
    ???
