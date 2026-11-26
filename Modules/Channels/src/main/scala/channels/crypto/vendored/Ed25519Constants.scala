// Derived from Tink (https://github.com/google/tink, java_src), via the Scala port
// https://github.com/ckuessner/unofficial-tink-scala (commit ed64d86a25f0e38688dbcb23214974196a502706).
// Copyright 2017 Google Inc., licensed under the Apache License, Version 2.0
// (http://www.apache.org/licenses/LICENSE-2.0). Modified: moved package, no functional changes.
package channels.crypto.vendored

import java.math.BigInteger
import scala.compiletime.uninitialized

/** Constants used in {@link Ed25519}. */
object Ed25519Constants {

  // d = -121665 / 121666 mod 2^255-19
  private[vendored] var D: Array[Long] = uninitialized
  // 2d
  private[vendored] var D2: Array[Long] = uninitialized
  // 2^((p-1)/4) mod p where p = 2^255-19
  private[vendored] var SQRTM1: Array[Long] = uninitialized

  /**
   * Base point for the Edwards twisted curve = (x, 4/5) and its exponentiations. B_TABLE[i][j] =
   * (j+1)*256^i*B for i in [0, 32) and j in [0, 8). Base point B = B_TABLE[0][0]
   *
   * <p>See {@link Ed25519ConstantsGenerator}.
   */
  private[vendored] var B_TABLE: Array[Array[Ed25519.CachedXYT]] = uninitialized
  private[vendored] var B2: Array[Ed25519.CachedXYT] = uninitialized

  private val P_BI =
    BigInteger.valueOf(2).pow(255).subtract(BigInteger.valueOf(19))
  private val D_BI =
    BigInteger.valueOf(-121665).multiply(BigInteger.valueOf(121666).modInverse(P_BI)).mod(P_BI)
  private val D2_BI = BigInteger.valueOf(2).multiply(D_BI).mod(P_BI)
  private val SQRTM1_BI =
    BigInteger.valueOf(2).modPow(P_BI.subtract(BigInteger.ONE).divide(BigInteger.valueOf(4)), P_BI)

  private class Point {
    var x: BigInteger = uninitialized
    var y: BigInteger = uninitialized
  }

  private def recoverX(y: BigInteger): BigInteger = {
    // x^2 = (y^2 - 1) / (d * y^2 + 1) mod 2^255-19
    val xx =
      y.pow(2)
        .subtract(BigInteger.ONE)
        .multiply(D_BI.multiply(y.pow(2)).add(BigInteger.ONE).modInverse(P_BI))
    var x: BigInteger = xx.modPow(P_BI.add(BigInteger.valueOf(3)).divide(BigInteger.valueOf(8)), P_BI)
    if !(x.pow(2).subtract(xx).mod(P_BI) == BigInteger.ZERO) then {
      x = x.multiply(SQRTM1_BI).mod(P_BI)
    }
    if x.testBit(0) then {
      x = P_BI.subtract(x)
    }
    x
  }

  private def edwards(a: Ed25519Constants.Point, b: Ed25519Constants.Point) = {
    val o = new Ed25519Constants.Point
    val xxyy = D_BI.multiply(a.x.multiply(b.x).multiply(a.y).multiply(b.y)).mod(P_BI)
    o.x =
      (a.x.multiply(b.y).add(b.x.multiply(a.y)))
        .multiply(BigInteger.ONE.add(xxyy).modInverse(P_BI))
        .mod(P_BI)
    o.y =
      (a.y.multiply(b.y).add(a.x.multiply(b.x)))
        .multiply(BigInteger.ONE.subtract(xxyy).modInverse(P_BI))
        .mod(P_BI)
    o
  }

  private def toLittleEndian(n: BigInteger): Array[Byte] = {
    val b = new Array[Byte](32)
    val nBytes = n.toByteArray
    System.arraycopy(nBytes, 0, b, 32 - nBytes.length, nBytes.length)
    for i <- 0 until b.length / 2 do {
      val t: Byte = b(i)
      b(i) = b(b.length - i - 1)
      b(b.length - i - 1) = t
    }
    b
  }

  private def getCachedXYT(p: Ed25519Constants.Point): Ed25519.CachedXYT = {
    new Ed25519.CachedXYT(
      Field25519.expand(toLittleEndian(p.y.add(p.x).mod(P_BI))),
      Field25519.expand(toLittleEndian(p.y.subtract(p.x).mod(P_BI))),
      Field25519.expand(toLittleEndian(D2_BI.multiply(p.x).multiply(p.y).mod(P_BI))))
  }

  {
    val b: Point = new Ed25519Constants.Point
    b.y = BigInteger.valueOf(4).multiply(BigInteger.valueOf(5).modInverse(P_BI)).mod(P_BI)
    b.x = recoverX(b.y)

    D = Field25519.expand(toLittleEndian(D_BI))
    D2 = Field25519.expand(toLittleEndian(D2_BI))
    SQRTM1 = Field25519.expand(toLittleEndian(SQRTM1_BI))

    var bi: Point = b
    B_TABLE = Array.ofDim[Ed25519.CachedXYT](32, 8)
    for i <- 0 until 32 do {
      var bij: Point = bi
      for j <- 0 until 8 do {
        B_TABLE(i)(j) = getCachedXYT(bij)
        bij = edwards(bij, bi)
      }
      for j <- 0 until 8 do {
        bi = edwards(bi, bi)
      }
    }
    bi = b
    val b2: Ed25519Constants.Point = edwards(b, b)
    B2 = new Array[Ed25519.CachedXYT](8)
    for i <- 0 until 8 do {
      B2(i) = getCachedXYT(bi)
      bi = edwards(bi, b2)
    }
  }
}
