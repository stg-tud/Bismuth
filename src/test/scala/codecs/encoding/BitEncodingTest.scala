package de.tu_darmstadt.stg.daimpl
package codecs.encoding

import org.scalatest.flatspec.AnyFlatSpec

class BitEncodingTest extends AnyFlatSpec {
  "fromString" should "work for Encoding" in {
    assert(BitEncoding.fromString("0") == BitEncoding().add(0, 1))
    assert(BitEncoding.fromString("1") == BitEncoding().add(1, 1))
    assert(BitEncoding.fromString("010") == BitEncoding().add(2, 3))
    assert(BitEncoding.fromString("1001") == BitEncoding().add(1, 2).add(2, 2))
  }

  "toString" should "work for Encoding" in {
    assert(BitEncoding().add(0, 1).toString() == "0")
    assert(BitEncoding().add(1, 1).toString() == "1")
    assert(BitEncoding().add(2, 3).toString() == "010")
    assert(BitEncoding().add(1, 2).add(2, 2).toString() == "1001")
  }

  "add" should "work for Encoding" in {
    assert(BitEncoding().add(0, 1) + BitEncoding().add(1, 1) == BitEncoding().add(0, 1).add(1, 1))
    assert(BitEncoding().add(0, 1).add(1, 1) + BitEncoding().add(0, 1) == BitEncoding().add(0, 1).add(1, 1).add(0, 1))
    assert(BitEncoding().add(0, 1).add(1, 1) + BitEncoding().add(1, 1).add(0, 1) == BitEncoding().add(0, 1).add(1, 1).add(1, 1).add(0, 1))
  }

  it should "work for empty encodings" in {
    assert(BitEncoding() + BitEncoding() == BitEncoding())
  }

  "get" should "work for Encoding" in {
    val enc = BitEncoding.fromString("10010")
    assert(enc.get(2) == 2)
    assert(enc.get(3) == 2)
    assert(enc.get(5) == 18)
  }

  "del" should "work for Encoding" in {
    assert(BitEncoding.fromString("10010").del(1) == BitEncoding.fromString("1001"))
    assert(BitEncoding.fromString("10010").del(2) == BitEncoding.fromString("100"))
    assert(BitEncoding.fromString("10010").del(4) == BitEncoding.fromString("1"))
  }
}
