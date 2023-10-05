package com.github.ckuessner
package codecs.internal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class BitWriterTest extends AnyFlatSpec {
  "write" should "work with Int.MaxValue" in {
    val bitWriter = BitWriter.empty
    bitWriter.write(Int.MaxValue, 31)
    bitWriter.toByteArray shouldBe byteArray(0xff, 0xff, 0xff, 0xfe)
  }

  it should "work with Int.MaxValue in unaligned case" in {
    var bitWriter = BitWriter.empty
    bitWriter.write(1, 1)
    bitWriter.write(Int.MaxValue, 31)
    bitWriter.toByteArray shouldBe byteArray(0xff, 0xff, 0xff, 0xff)

    bitWriter = BitWriter.empty
    bitWriter.write(Int.MaxValue, 31)
    bitWriter.write(1, 1)
    bitWriter.toByteArray shouldBe byteArray(0xff, 0xff, 0xff, 0xff)
  }

  it should "throw if number >= 2^bits)" in {
    var bitWriter = BitWriter.empty
    assertThrows[AssertionError](bitWriter.write(10, 1))

    bitWriter = BitWriter.empty
    assertThrows[AssertionError](bitWriter.write(10, 2))

    bitWriter = BitWriter.empty
    assertThrows[AssertionError](bitWriter.write(10, 3))

    bitWriter = BitWriter.empty
    bitWriter.write(10, 4)

    bitWriter = BitWriter.empty
    bitWriter.write(31, 5)

    bitWriter = BitWriter.empty
    assertThrows[AssertionError](bitWriter.write(32, 5))
  }

  private def byteArray(values: Int*): Array[Byte] = values.map(_.toByte).toArray[Byte]
}
