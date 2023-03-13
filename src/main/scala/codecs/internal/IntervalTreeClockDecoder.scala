package de.tu_darmstadt.stg.daimpl
package codecs.internal

import causality.IntervalTreeClock

private[codecs] object IntervalTreeClockDecoder {
  def decode(byteArray: Array[Byte]): IntervalTreeClock = {
    decode(BitReader(byteArray))
  }

  @throws[MatchError]
  def decode(bitReader: BitReader): IntervalTreeClock = {
    val idTree    = IdTreeDecoder.decode(bitReader)
    val eventTree = EventTreeDecoder.decode(bitReader)
    IntervalTreeClock(idTree, eventTree)
  }
}
