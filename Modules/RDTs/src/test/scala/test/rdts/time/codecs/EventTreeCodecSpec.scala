package test.rdts.time.codecs

import rdts.time.EventTree
import rdts.time.codecs.EventTreeCodec
import test.rdts.time.EventTreeGenerators.genEventTree

class EventTreeCodecSpec extends CodecSpec[EventTree] {
  def printByteArray(array: Array[Byte]): Unit = {
    val strings = array.map { byte =>
      val (left, right) = Integer.toBinaryString(byte & 0xff).padTo(8, '0').splitAt(4)
      left + '_' + right
    }
    println(strings.mkString("Array(", ", ", ")"))
  }
}
