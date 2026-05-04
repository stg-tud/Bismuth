package com.github.ckuessner
package codecs

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import java.nio.ByteBuffer

trait CodecSpec[T: {Codec, Gen}] extends FunSuite with ScalaCheckSuite {
  val encoder: Codec[T] = summon[Codec[T]]
  val tGen: Gen[T]      = summon[Gen[T]]

  property("readArray should be the inverse of writeArray") {
    forAll(tGen) { (obj: T) =>
      val encoded = encoder.writeArray(obj)
      val decoded = encoder.readArray(encoded)
      assertEquals(decoded, obj)
    }
  }
}
