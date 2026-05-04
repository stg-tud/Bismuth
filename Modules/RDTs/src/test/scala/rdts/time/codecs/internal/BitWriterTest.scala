package rdts.time.codecs.internal

import munit.FunSuite

class BitWriterTest extends FunSuite {

  test("write works with Int.MaxValue") {
    val bitWriter = BitWriter.empty
    bitWriter.write(Int.MaxValue, 31)
    assertEquals(bitWriter.toByteArray.toSeq, byteArray(0xff, 0xff, 0xff, 0xfe).toSeq)
  }

  test("write works with Int.MaxValue in unaligned case") {
    var bitWriter = BitWriter.empty
    bitWriter.write(1, 1)
    bitWriter.write(Int.MaxValue, 31)
    assertEquals(bitWriter.toByteArray.toSeq, byteArray(0xff, 0xff, 0xff, 0xff).toSeq)

    bitWriter = BitWriter.empty
    bitWriter.write(Int.MaxValue, 31)
    bitWriter.write(1, 1)
    assertEquals(bitWriter.toByteArray.toSeq, byteArray(0xff, 0xff, 0xff, 0xff).toSeq)
  }

  test("write throws if number >= 2^bits") {
    var bitWriter = BitWriter.empty
    intercept[AssertionError](bitWriter.write(10, 1))

    bitWriter = BitWriter.empty
    intercept[AssertionError](bitWriter.write(10, 2))

    bitWriter = BitWriter.empty
    intercept[AssertionError](bitWriter.write(10, 3))

    bitWriter = BitWriter.empty
    bitWriter.write(10, 4)

    bitWriter = BitWriter.empty
    bitWriter.write(31, 5)

    bitWriter = BitWriter.empty
    intercept[AssertionError](bitWriter.write(32, 5))
  }

  private def byteArray(values: Int*): Array[Byte] = values.map(_.toByte).toArray[Byte]
}
