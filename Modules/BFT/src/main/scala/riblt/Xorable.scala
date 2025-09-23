package riblt

trait Xorable[A]:
  extension (a1: A) def xor(a2: A): A
  extension (a: A) def removeTrailingZeros(): A
  def zero: A

given Xorable[Int]:
  extension (i1: Int) override def xor(i2: Int): Int =
      i1 ^ i2
  extension (a: Int) override def removeTrailingZeros(): Int = a

  override def zero: Int = 0

given Xorable[String]:
  extension (str1: String) override def xor(str2: String): String =
    val byteArray1 = str1.getBytes
    val byteArray2 = str2.getBytes

    // Ensure both byte arrays are of the same length
    val maxLength = math.max(byteArray1.length, byteArray2.length)
    val paddedByteArray1 = byteArray1.padTo(maxLength, 0.toByte)
    val paddedByteArray2 = byteArray2.padTo(maxLength, 0.toByte)

    val resultBytes = paddedByteArray1.zip(paddedByteArray2).map { case (b1, b2) => (b1 ^ b2).toByte }

    String(resultBytes)

  extension (a: String) override def removeTrailingZeros(): String =
    val bytes = a.getBytes
    val lastNonZeroIndex = bytes.lastIndexWhere(_ != 0)
    if (lastNonZeroIndex == -1) String(Array.emptyByteArray)
    else String(bytes.slice(0, lastNonZeroIndex + 1))

  override def zero: String = String(Array.fill(1)(0.toByte))

