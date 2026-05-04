package rdts.time.codecs.internal

import rdts.time.IntervalTreeClock

private[codecs] object IntervalTreeClockEncoder {
  def encode(itc: IntervalTreeClock): Array[Byte] = {
    val bitWriter = BitWriter.empty
    encode(itc, bitWriter)
    bitWriter.toByteArray
  }

  def encode(itc: IntervalTreeClock, bitWriter: BitWriter): Unit = {
    IdTreeEncoder.encode(itc.idTree, bitWriter)
    EventTreeEncoder.encode(itc.eventTree, bitWriter)
  }
}
