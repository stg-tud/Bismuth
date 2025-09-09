import riblt.{CodedSymbol, CodingWindow, Decoder, Encoder, Hashable, HashedSymbol, Mapping, Xorable, ord}

import scala.collection.mutable
import scala.util.control.Breaks.break
import scala.util.hashing.MurmurHash3

given Hashable[Int]:
  extension (a: Int)
    override def hash: Long =
      MurmurHash3.stringHash(a.toString) // + "add some salt to the mix")

given Xorable[Int]:
  extension (a1: Int)
    override def xor(a2: Int): Int =
      a1 ^ a2

  extension (a: Int) override def zero: Int = 0

given Int = 0

class RIBLTTest extends munit.FunSuite:
  test("test") {
    val alice = List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val bob   = List[Int](1, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    val enc = Encoder[Int]()
    for s <- alice do
      enc.addSymbol(s)

    val dec = Decoder[Int]()
    for s <- bob do
      dec.addSymbol(s)

    var i = 0
    var d = true
    while d do
      val s = enc.produceNextCodedSymbol
      i += 1
      dec.addCodedSymbol(s)
      dec.tryDecode
      if dec.isDecoded then
        d = false

    print(s"${dec.remoteSymbols.head.symbol} is exclusive to Alice")
    print(s"\n${dec.localSymbols.head.symbol} is exclusive to Bob")
    print(s"\n$i coded symbols sent")
  }
