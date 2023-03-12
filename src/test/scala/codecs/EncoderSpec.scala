package de.tu_darmstadt.stg.daimpl
package codecs

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.ByteBuffer

trait EncoderSpec[T: Encoder: Gen] extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  val encoder: Encoder[T] = summon[Encoder[T]]
  val tGen: Gen[T]        = summon[Gen[T]]

  "readArray" should "be the inverse of writeArray" in {
    forAll(tGen) { (obj: T) =>
      val encoded = encoder.writeArray(obj)
      val decoded = encoder.readArray(encoded)
      decoded shouldBe obj
    }
  }

  "readObjectArray" should "be the inverse of writeObjectArray" in {
    forAll(tGen) { (obj: T) =>
      val encoded = encoder.writeObjectArray(obj)
      val decoded = encoder.readObjectArray(encoded)
      decoded shouldBe obj
    }
  }
}

trait FixedSizeEncoderSpec[T: FixedSizeEncoder] extends EncoderSpec[T] {
  val fixedSizeEncoder: FixedSizeEncoder[T] = summon[FixedSizeEncoder[T]]

  "read" should "be the inverse of write" in {
    forAll(tGen) { (obj: T) =>
      val bufferSize = fixedSizeEncoder.BYTES
      val byteBuffer = ByteBuffer.allocate(bufferSize)
      encoder.write(obj, byteBuffer)
      val readOnlyBuffer = ByteBuffer.wrap(byteBuffer.array()).asReadOnlyBuffer()
      val decoded        = encoder.read(readOnlyBuffer, bufferSize)
      decoded shouldBe obj
    }
  }
}

trait VariableSizeEncoderSpec[T: VariableSizeEncoder] extends EncoderSpec[T] {
  val variableSizeEncoder: VariableSizeEncoder[T] = summon[VariableSizeEncoder[T]]

  "read" should "be the inverse of write" in {
    forAll(tGen) { (obj: T) =>
      val bufferSize = variableSizeEncoder.BYTES(obj)
      val byteBuffer = ByteBuffer.allocate(bufferSize)
      encoder.write(obj, byteBuffer)
      val readOnlyBuffer = ByteBuffer.wrap(byteBuffer.array()).asReadOnlyBuffer()
      val decoded        = encoder.read(readOnlyBuffer, bufferSize)
      decoded shouldBe obj
    }
  }
}
