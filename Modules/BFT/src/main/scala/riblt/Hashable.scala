package riblt

import scala.util.hashing.MurmurHash3

trait Hashable[A]:
  extension (a: A) def hash: Long


given Hashable[Int]:
  extension (a: Int) override def hash: Long =
    MurmurHash3.stringHash(a.toString)

given Hashable[String]:
  extension (a: String) override def hash: Long =
    MurmurHash3.stringHash(a)
