package com.github.ckuessner
package codecs.internal

import codecs.internal.BitReaderTest.{byte, int}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class BitReaderTest extends AnyFlatSpec {
  "readNextIntOfLength" should "work with 0xFF" in {
    val reader = new BitReader(Array(0xFF.byteValue()))
    reader.readNextIntOfLength(8) shouldBe 0xff
    reader.resetPos()

    reader.readNextIntOfLength(5) shouldBe 0x1f
    reader.resetPos()

    reader.readNextBit()
    reader.readNextIntOfLength(5) shouldBe 0x1f
  }

  it should "work with byte-boundary spanning length" in {
    val reader  = new BitReader(Array(byte("10100001"), byte("10001111")))
    val allBits = "1010000110001111"

    for (startBit <- 0 until 8) {
      for (lengthOfInt <- 1 until (16 - startBit)) {
        for (_ <- 0 until startBit) {
          reader.readNextBit()
        }
        reader.readNextIntOfLength(lengthOfInt) shouldBe int(allBits.substring(startBit, startBit + lengthOfInt))
        reader.resetPos()
      }
    }

  }
}

private[this] object BitReaderTest {
  def byte(s: String): Byte = {
    java.lang.Integer.parseInt(s, 2).byteValue
  }

  def int(s: String): Int = {
    java.lang.Integer.parseInt(s, 2)
  }
}
