package riblt

import riblt.Operation.{Add, Remove}
import scala.util.hashing.MurmurHash3

class RIBLT[T](
    var receivedCodedSymbols: List[CodedSymbol[T]],
    var local: CodingWindow[T],
    var window: CodingWindow[T],
    var remote: CodingWindow[T],
    var decodable: List[CodedSymbol[T]],
):

  def isDecoded: Boolean =
    receivedCodedSymbols.nonEmpty && receivedCodedSymbols.forall(c => c.decoded)

  def localSymbols: List[SourceSymbol[T]] =
    local.symbols

  def remoteSymbols: List[SourceSymbol[T]] =
    remote.symbols

  def addSymbol(symbol: T)(using Hashable[T]): Unit =
    window.addSourceSymbol(symbol)

  def produceNextCodedSymbol(using Xorable[T]): CodedSymbol[T] =
    window.produceNextCodedSymbol

  def produceNextCodedSymbols(count: Int = 1)(using Xorable[T]): List[CodedSymbol[T]] =
    var result = List.empty[CodedSymbol[T]]

    for i <- 0 to count do
      result = result :+ produceNextCodedSymbol

    result

  def addCodedSymbol(codedSymbol: CodedSymbol[T])(using Xorable[T])(using Hashable[T]): Unit =
    var c = window.applyCodedSymbol(codedSymbol, Remove)
    c = remote.applyCodedSymbol(c, Remove)
    c = local.applyCodedSymbol(c, Add)

    receivedCodedSymbols = receivedCodedSymbols :+ c

    if c.count == 1 || c.count == -1 then
      c.sum = c.sum.removeTrailingZeros()

    if (c.count == 1 || c.count == -1) && (c.hash == c.sum.hash) then
      decodable = decodable :+ c
    else if c.count == 0 && c.hash == 0 then
      decodable = decodable :+ c

    tryDecode

  def addCodedSymbols(codedSymbol: List[CodedSymbol[T]])(using Xorable[T])(using Hashable[T]): Unit =
    for codedSymbol <- receivedCodedSymbols do
      addCodedSymbol(codedSymbol)

  def applyNewSymbol(sourceSymbol: SourceSymbol[T], op: Operation)(using
      Hashable[T]
  )(using Xorable[T]): SourceSymbol[T] =
    var i = sourceSymbol.mapping.lastIndex.toInt
    while i < receivedCodedSymbols.length do
      val tmp = op match
        case Add    => receivedCodedSymbols(i).add(sourceSymbol)
        case Remove => receivedCodedSymbols(i).remove(sourceSymbol)

      receivedCodedSymbols = receivedCodedSymbols.updated(i, tmp)

      if receivedCodedSymbols(i).count == -1 || receivedCodedSymbols(i).count == 1 then
        receivedCodedSymbols(i).sum = receivedCodedSymbols(i).sum.removeTrailingZeros()
        if receivedCodedSymbols(i).hash == receivedCodedSymbols(i).sum.hash then
          decodable = decodable :+ receivedCodedSymbols(i)

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

object RIBLT:
  def apply[T](): RIBLT[T] =
    new RIBLT(List.empty, CodingWindow(), CodingWindow(), CodingWindow(), List.empty)

  def empty[T]: RIBLT[T] = RIBLT()

  given Hashable[Int]:
    extension (a: Int)
      override def hash: Long =
        MurmurHash3.stringHash(a.toString)

  given Hashable[Long]:
    extension (a: Long)
      override def hash: Long =
        MurmurHash3.stringHash(a.toString)

  given Hashable[String]:
    extension (a: String)
      override def hash: Long =
        MurmurHash3.stringHash(a)

  given Hashable[Array[Byte]]:
    extension (a: Array[Byte])
      override def hash: Long =
        MurmurHash3.bytesHash(a)

  given Xorable[Int]:
    extension (i1: Int)
      override def xor(i2: Int): Int =
        i1 ^ i2
    extension (a: Int) override def removeTrailingZeros(): Int = a

    override def zero: Int = 0

  given Xorable[Long]:
    extension (i1: Long)
      override def xor(i2: Long): Long =
        i1 ^ i2
    extension (a: Long) override def removeTrailingZeros(): Long = a

    override def zero: Long = 0

  given Xorable[String]:
    extension (str1: String)
      override def xor(str2: String): String =
        val byteArray1 = str1.getBytes
        val byteArray2 = str2.getBytes

        // Ensure both byte arrays are of the same length
        val maxLength        = math.max(byteArray1.length, byteArray2.length)
        val paddedByteArray1 = byteArray1.padTo(maxLength, 0.toByte)
        val paddedByteArray2 = byteArray2.padTo(maxLength, 0.toByte)

        val resultBytes = paddedByteArray1.zip(paddedByteArray2).map { case (b1, b2) => (b1 ^ b2).toByte }

        String(resultBytes)

    extension (a: String)
      override def removeTrailingZeros(): String =
        val bytes            = a.getBytes
        val lastNonZeroIndex = bytes.lastIndexWhere(_ != 0)
        if lastNonZeroIndex == -1 then a
        else String(bytes.slice(0, lastNonZeroIndex + 1))

    override def zero: String = String(Array.fill(1)(0.toByte))

  given Xorable[Array[Byte]]:
    extension (b1: Array[Byte])
      override def xor(b2: Array[Byte]): Array[Byte] =
        // Ensure both byte arrays are of the same length
        val maxLength        = math.max(b1.length, b2.length)
        val paddedByteArray1 = b1.padTo(maxLength, 0.toByte)
        val paddedByteArray2 = b2.padTo(maxLength, 0.toByte)

        val resultBytes = paddedByteArray1.zip(paddedByteArray2).map { case (b1, b2) => (b1 ^ b2).toByte }

        resultBytes

    extension (b: Array[Byte])
      override def removeTrailingZeros(): Array[Byte] =
        val lastNonZeroIndex = b.lastIndexWhere(_ != 0)
        if lastNonZeroIndex == -1 then b
        else b.slice(0, lastNonZeroIndex + 1)

    override def zero: Array[Byte] = Array.fill(1)(0.toByte)
