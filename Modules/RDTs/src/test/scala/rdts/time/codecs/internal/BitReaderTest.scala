package com.github.ckuessner
package codecs.internal

import codecs.internal.BitReaderTest.{byte, int}
import munit.FunSuite

class BitReaderTest extends FunSuite {

  test("readNextIntOfLength works with 0xFF") {
    val reader = new BitReader(Array(0xff.byteValue()))
    assertEquals(reader.readNextIntOfLength(8), 0xff)
    reader.resetPos()

    assertEquals(reader.readNextIntOfLength(5), 0x1f)
    reader.resetPos()

    reader.readNextBit()
    assertEquals(reader.readNextIntOfLength(5), 0x1f)
  }

  test("readNextIntOfLength works with byte-boundary spanning length") {
    val reader  = new BitReader(Array(byte("10100001"), byte("10001111")))
    val allBits = "1010000110001111"

    for startBit <- 0 until 8 do {
      for lengthOfInt <- 1 until (16 - startBit) do {
        for _ <- 0 until startBit do
            reader.readNextBit()
        assertEquals(reader.readNextIntOfLength(lengthOfInt), int(allBits.substring(startBit, startBit + lengthOfInt)))
        reader.resetPos()
      }
    }
  }
}

private object BitReaderTest {
  def byte(s: String): Byte =
    java.lang.Integer.parseInt(s, 2).byteValue

  def int(s: String): Int =
    java.lang.Integer.parseInt(s, 2)
}
