package riblt

import scala.math._

class Mapping(
               var prng: BigInt,
               var lastIndex: BigInt = 0L,
               val c: BigInt = BigInt("da942042e4dd58b5", 16)
             ):

  def nextIndex: BigInt =
    val r = prng * c
    prng = r


    val u = (1L << 32).toDouble / sqrt(r.toDouble + 1)
    val diff = ceil((lastIndex.toDouble + 1.5) * (u - 1.0)).toLong

    if diff < 0 then
      lastIndex += diff * -1
    else
      lastIndex += diff

    lastIndex

object Mapping:
  def apply(bytes: Array[Byte]): Mapping =
    new Mapping(BigInt(1, bytes))



class SymbolMapping (
                      var sourceIndex: Int,
                      var codedIndex: Int
                    )
