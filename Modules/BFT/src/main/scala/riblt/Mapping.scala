package riblt

import scala.math.*
import scala.util.hashing.MurmurHash3

class Mapping(
    var prng: Long,
    var lastIndex: Long = 0L,
    val c: Long = BigInt("da942042e4dd58b5", 16).toLong
):

   def nextIndex: Long =
      var r = prng * c
      if r < 0 then
         r = r * -1

      prng = r

      val u    = (1L << 32).toDouble / sqrt(r.toDouble + 1)
      val diff = ceil((lastIndex.toDouble + 1.5) * (u - 1.0)).toLong

      if diff < 0 then
         lastIndex += diff * -1
      else
         lastIndex += diff

      lastIndex

object Mapping:
   def apply(bytes: Array[Byte]): Mapping =
     new Mapping(MurmurHash3.arrayHash(bytes))

//  private def murmurHash64(bytes: Array[Byte]): Long =
//    val seed = 0x9747b28c
//
//    val hash1x32 = MurmurHash3.bytesHash(bytes, seed)
//    val hash1    = hash1x32.toLong & 0xffffffffL
//    val hash2    = MurmurHash3.bytesHash(bytes, hash1x32).toLong & 0xffffffffL
//
//    (hash1 << 32) | hash2

class SymbolMapping(
    var sourceIndex: Int,
    var codedIndex: Int
)
