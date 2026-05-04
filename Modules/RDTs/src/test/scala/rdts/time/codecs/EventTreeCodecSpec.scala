package rdts.time.codecs

import rdts.time.EventTree
import rdts.time.EventTree.{Branch, Leaf}
import rdts.time.EventTreeGenerators.genEventTree
import rdts.time.codecs.{CodecSpec, EventTreeCodec}

class EventTreeCodecSpec extends CodecSpec[EventTree] {
  def printByteArray(array: Array[Byte]): Unit = {
    val strings = array.map { byte =>
      val (left, right) = Integer.toBinaryString(byte & 0xff).padTo(8, '0').splitAt(4)
      left + '_' + right
    }
    println(strings.mkString("Array(", ", ", ")"))
  }
}
