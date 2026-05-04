package com.github.ckuessner
package codecs

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import java.nio.ByteBuffer

trait EncoderSpec[T: {Encoder, Gen}] extends FunSuite with ScalaCheckSuite {
  val encoder: Encoder[T] = summon[Encoder[T]]
  val tGen: Gen[T]        = summon[Gen[T]]

  property("readArray should be the inverse of writeArray") {
    forAll(tGen) { (obj: T) =>
      val encoded = encoder.writeArray(obj)
      val decoded = encoder.readArray(encoded)
      assertEquals(decoded, obj)
    }
  }
}

trait FixedSizeEncoderSpec[T: FixedSizeEncoder] extends EncoderSpec[T] {
  val fixedSizeEncoder: FixedSizeEncoder[T] = summon[FixedSizeEncoder[T]]

  property("read should be the inverse of write") {
    forAll(tGen) { (obj: T) =>
      val bufferSize = fixedSizeEncoder.BYTES
      val byteBuffer = ByteBuffer.allocate(bufferSize)
      encoder.write(obj, byteBuffer)
      val readOnlyBuffer = ByteBuffer.wrap(byteBuffer.array()).asReadOnlyBuffer()
      val decoded        = encoder.read(readOnlyBuffer, bufferSize)
      assertEquals(decoded, obj)
    }
  }
}

trait VariableSizeEncoderSpec[T: VariableSizeEncoder] extends EncoderSpec[T] {
  val variableSizeEncoder: VariableSizeEncoder[T] = summon[VariableSizeEncoder[T]]

  property("read should be the inverse of write") {
    forAll(tGen) { (obj: T) =>
      val bufferSize = variableSizeEncoder.BYTES(obj)
      val byteBuffer = ByteBuffer.allocate(bufferSize)
      encoder.write(obj, byteBuffer)
      val readOnlyBuffer = ByteBuffer.wrap(byteBuffer.array()).asReadOnlyBuffer()
      val decoded        = encoder.read(readOnlyBuffer, bufferSize)
      assertEquals(decoded, obj)
    }
  }
}
