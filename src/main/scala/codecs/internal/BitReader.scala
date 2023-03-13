package de.tu_darmstadt.stg.daimpl
package codecs.internal

private[internal] class BitReader(private val byteArray: Array[Byte]) {
  private var pos = 0

  def readNextBit(): Int = {
    val byte: Int = byteArray(pos / 8) & 0xff
    val bit       = (byte >> (7 - (pos % 8))) & 0x01
    pos += 1
    bit
  }

  def readNextTwoBits(): Int = {
    if (pos % 8 != 7) {
      // Both bits in same byte
      val byte: Int = byteArray(pos / 8) & 0xff
      val bits      = (byte >>> (6 - (pos % 8))) & 0x03 // shift so that considered bits are in lowest bit-pos
      pos += 2
      bits
    } else {
      // Bits spanning across the byte boundary
      val firstByte: Int  = byteArray(pos / 8) & 0xff
      val secondByte: Int = byteArray((pos / 8) + 1) & 0xff
      val bits            = ((firstByte << 1) | (secondByte >>> 7)) & 0x03
      pos += 2
      bits
    }
  }

  def readNextIntOfLength(numBits: Int): Int = {
    if (pos % 8 == 0) return readNextBoundaryAlignedIntOfLength(numBits)

    val numUnreadBitsInByte     = 8 - (pos % 8)
    val unreadBitMask           = (1 << numUnreadBitsInByte) - 1
    val unreadBits              = byteArray(pos / 8) & unreadBitMask
    val numTrailingBitsToIgnore = math.max(0, numUnreadBitsInByte - numBits)

    val result = unreadBits >>> numTrailingBitsToIgnore
    pos += numUnreadBitsInByte - numTrailingBitsToIgnore
    val remainingBits = numBits - (numUnreadBitsInByte - numTrailingBitsToIgnore)

    if (remainingBits == 0) {
      result
    } else {
      (result << remainingBits) | readNextBoundaryAlignedIntOfLength(remainingBits)
    }
  }

  private inline def readNextBoundaryAlignedIntOfLength(numBits: Int): Int = {
    var result: Int   = 0
    var remainingBits = numBits

    while (remainingBits > 0) {
      val byte: Int = byteArray(pos / 8) & 0xff

      if (remainingBits < 8) {
        result = result << remainingBits
        result = result | (byte >>> (8 - remainingBits))

        pos += remainingBits
        remainingBits = 0
      } else if (numBits >= 8) {
        result = result << 8
        result = result | byte

        pos += 8
        remainingBits -= 8

      }
    }

    result
  }

  /** Consume every bit to the first 0 (including the 0).
    *
    * @return
    *   The count of bits consumed (including the 0)
    */
  def readToNextZeroAndReturnNumberOfBitsRead(): Int = {
    val posBeforeFirstBitRead = pos
    while (readNextBit() == 1) {}
    pos - posBeforeFirstBitRead
  }

  def resetPos(): Unit = {
    pos = 0
  }

}
