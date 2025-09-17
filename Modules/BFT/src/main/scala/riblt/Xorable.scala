package riblt

import scala.util.hashing.MurmurHash3

trait Xorable[A]:
  extension (a1: A) def xor(a2: A): A
  def zero: A

given Hashable[Int]:
  extension (a: Int)
    override def hash: Long =
      MurmurHash3.stringHash(a.toString)

given Xorable[Int]:
  extension (a1: Int)
    override def xor(a2: Int): Int =
      a1 ^ a2

  override def zero: Int = 0