// Derived from Tink (https://github.com/google/tink, java_src), via the Scala port
// https://github.com/ckuessner/unofficial-tink-scala (commit ed64d86a25f0e38688dbcb23214974196a502706).
// Copyright 2017 Google Inc., licensed under the Apache License, Version 2.0
// (http://www.apache.org/licenses/LICENSE-2.0). Modified: moved package, uses the vendored Sha512, added the public seed based API.
package channels.crypto.vendored

import channels.crypto.vendored.Ed25519Constants.*
import channels.crypto.vendored.Field25519.{FIELD_LEN, LIMB_CNT}

import java.util

/**
 * This implementation is based on the ed25519/ref10 implementation in NaCl.
 *
 * <p>It implements this twisted Edwards curve:
 *
 * <pre>
 * -x^2 + y^2 = 1 + (-121665 / 121666 mod 2^255-19)*x^2*y^2
 * </pre>
 *
 *
 * @see <a href="https://eprint.iacr.org/2008/013.pdf">Bernstein D.J., Birkner P., Joye M., Lange
 *      T., Peters C. (2008) Twisted Edwards Curves</a>
 * @see <a href="https://eprint.iacr.org/2008/522.pdf">Hisil H., Wong K.KH., Carter G., Dawson E.
 *      (2008) Twisted Edwards Curves Revisited</a>
 */
object Ed25519 {

  val SECRET_KEY_LEN: Int = FIELD_LEN
  val PUBLIC_KEY_LEN: Int = FIELD_LEN
  val SIGNATURE_LEN: Int = FIELD_LEN * 2

  // (x = 0, y = 1) point
  private val CACHED_NEUTRAL: CachedXYT = new Ed25519.CachedXYT(
    Array[Long](1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array[Long](1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array[Long](0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  private val NEUTRAL = new Ed25519.PartialXYZT(
    new Ed25519.XYZ(Array[Long](0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array[Long](1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array[Long](1, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    Array[Long](1, 0, 0, 0, 0, 0, 0, 0, 0, 0))

  /**
   * Projective point representation (X:Y:Z) satisfying x = X/Z, y = Y/Z
   *
   * Note that this is referred as ge_p2 in ref10 impl.
   * Also note that x = X, y = Y and z = Z below following Java coding style.
   *
   * See
   * Koyama K., Tsuruoka Y. (1993) Speeding up Elliptic Cryptosystems by Using a Signed Binary
   * Window Method.
   *
   * https://hyperelliptic.org/EFD/g1p/auto-twisted-projective.html
   */
  private[Ed25519] class XYZ private[Ed25519](private[Ed25519] final val x: Array[Long] = new Array[Long](LIMB_CNT),
                                    private[Ed25519] final val y: Array[Long] = new Array[Long](LIMB_CNT),
                                    private[Ed25519] final val z: Array[Long] = new Array[Long](LIMB_CNT)) {

    def this(xyz: Ed25519.XYZ) = {
      this(
        util.Arrays.copyOf(xyz.x, LIMB_CNT),
        util.Arrays.copyOf(xyz.y, LIMB_CNT),
        util.Arrays.copyOf(xyz.z, LIMB_CNT)
      )
    }

    def this(partialXYZT: Ed25519.PartialXYZT) = {
      this()
      XYZ.fromPartialXYZT(this, partialXYZT)
    }

    /**
     * Encodes this point to bytes.
     */
    private[vendored] def toBytes = {
      val recip = new Array[Long](LIMB_CNT)
      val x = new Array[Long](LIMB_CNT)
      val y = new Array[Long](LIMB_CNT)
      Field25519.inverse(recip, z)
      Field25519.mult(x, this.x, recip)
      Field25519.mult(y, this.y, recip)
      val s = Field25519.contract(y)
      s(31) = (s(31) ^ (getLsb(x) << 7)).toByte
      s
    }

    /** Checks that the point is on curve */
    private[vendored] def isOnCurve = {
      val x2 = new Array[Long](LIMB_CNT)
      Field25519.square(x2, x)
      val y2 = new Array[Long](LIMB_CNT)
      Field25519.square(y2, y)
      val z2 = new Array[Long](LIMB_CNT)
      Field25519.square(z2, z)
      val z4 = new Array[Long](LIMB_CNT)
      Field25519.square(z4, z2)
      val lhs = new Array[Long](LIMB_CNT)
      // lhs = y^2 - x^2
      Field25519.sub(lhs, y2, x2)
      // lhs = z^2 * (y2 - x2)
      Field25519.mult(lhs, lhs, z2)
      val rhs = new Array[Long](LIMB_CNT)
      // rhs = x^2 * y^2
      Field25519.mult(rhs, x2, y2)
      // rhs = D * x^2 * y^2
      Field25519.mult(rhs, rhs, D)
      // rhs = z^4 + D * x^2 * y^2
      Field25519.sum(rhs, z4)
      // Field25519.mult reduces its output, but Field25519.sum does not, so we have to manually
      // reduce it here.
      Field25519.reduce(rhs, rhs)
      // z^2 (y^2 - x^2) == z^4 + D * x^2 * y^2
      util.Arrays.equals(Field25519.contract(lhs), Field25519.contract(rhs))
    }
  }

  private object XYZ {
    /**
     * ge_p1p1_to_p2.c
     */
    //@CanIgnoreReturnValue
    private[vendored] def fromPartialXYZT(out: Ed25519.XYZ, in: Ed25519.PartialXYZT) = {
      Field25519.mult(out.x, in.xyz.x, in.t)
      Field25519.mult(out.y, in.xyz.y, in.xyz.z)
      Field25519.mult(out.z, in.xyz.z, in.t)
      out
    }
  }


  /**
   * Represents extended projective point representation (X:Y:Z:T) satisfying x = X/Z, y = Y/Z,
   * XY = ZT
   *
   * Note that this is referred as ge_p3 in ref10 impl.
   * Also note that t = T below following Java coding style.
   *
   * See
   * Hisil H., Wong K.KH., Carter G., Dawson E. (2008) Twisted Edwards Curves Revisited.
   *
   * https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html
   */
  private[Ed25519] class XYZT private[Ed25519](private[Ed25519] val xyz: Ed25519.XYZ = new Ed25519.XYZ,
                                               private[Ed25519] val t: Array[Long] = new Array[Long](LIMB_CNT)) {

    def this(partialXYZT: Ed25519.PartialXYZT) = {
      this()
      XYZT.fromPartialXYZT(this, partialXYZT)
    }
  }

  private object XYZT {
    /**
     * ge_p1p1_to_p2.c
     */
    //@CanIgnoreReturnValue
    private[Ed25519] def fromPartialXYZT(out: Ed25519.XYZT, in: Ed25519.PartialXYZT) = {
      Field25519.mult(out.xyz.x, in.xyz.x, in.t)
      Field25519.mult(out.xyz.y, in.xyz.y, in.xyz.z)
      Field25519.mult(out.xyz.z, in.xyz.z, in.t)
      Field25519.mult(out.t, in.xyz.x, in.xyz.y)
      out
    }

    /**
     * Decodes {@code s} into an extented projective point.
     * See Section 5.1.3 Decoding in https://tools.ietf.org/html/rfc8032#section-5.1.3
     */
    private[Ed25519] def fromBytesNegateVarTime(s: Array[Byte]) = {
      val x = new Array[Long](LIMB_CNT)
      val y = Field25519.expand(s)
      val z = new Array[Long](LIMB_CNT); z(0) = 1
      val t = new Array[Long](LIMB_CNT)
      val u = new Array[Long](LIMB_CNT)
      val v = new Array[Long](LIMB_CNT)
      val vxx = new Array[Long](LIMB_CNT)
      val check = new Array[Long](LIMB_CNT)
      Field25519.square(u, y)
      Field25519.mult(v, u, D)
      Field25519.sub(u, u, z) // u = y^2 - 1
      Field25519.sum(v, v, z) // v = dy^2 + 1

      val v3 = new Array[Long](LIMB_CNT)
      Field25519.square(v3, v)
      Field25519.mult(v3, v3, v) // v3 = v^3
      Field25519.square(x, v3)
      Field25519.mult(x, x, v)
      Field25519.mult(x, x, u) // x = uv^7

      pow2252m3(x, x) // x = (uv^7)^((q-5)/8)
      Field25519.mult(x, x, v3)
      Field25519.mult(x, x, u) // x = uv^3(uv^7)^((q-5)/8)

      Field25519.square(vxx, x)
      Field25519.mult(vxx, vxx, v)
      Field25519.sub(check, vxx, u) // vx^2-u
      if isNonZeroVarTime(check) then {
        Field25519.sum(check, vxx, u) // vx^2+u
        if isNonZeroVarTime(check) then {
          throw new IllegalArgumentException("Cannot convert given bytes to extended projective "
            + "coordinates. No square root exists for modulo 2^255-19")
        }
        Field25519.mult(x, x, SQRTM1)
      }
      if !isNonZeroVarTime(x) && (s(31) & 0xff) >> 7 != 0 then {
        throw new IllegalArgumentException("Cannot convert given bytes to extended projective "
          + "coordinates. Computed x is zero and encoded x's least significant bit is not zero")
      }
      if getLsb(x) == ((s(31) & 0xff) >> 7) then {
        neg(x, x)
      }

      Field25519.mult(t, x, y)
      new Ed25519.XYZT(new Ed25519.XYZ(x, y, z), t)
    }
  }

  /**
   * Partial projective point representation ((X:Z),(Y:T)) satisfying x=X/Z, y=Y/T
   *
   * Note that this is referred as complete form in the original ref10 impl (ge_p1p1).
   * Also note that t = T below following Java coding style.
   *
   * Although this has the same types as XYZT, it is redefined to have its own type so that it is
   * readable and 1:1 corresponds to ref10 impl.
   *
   * Can be converted to XYZT as follows:
   * X1 = X * T = x * Z * T = x * Z1
   * Y1 = Y * Z = y * T * Z = y * Z1
   * Z1 = Z * T = Z * T
   * T1 = X * Y = x * Z * y * T = x * y * Z1 = X1Y1 / Z1
   */
  private[vendored] class PartialXYZT private[Ed25519](final private[vendored] val xyz: Ed25519.XYZ = new Ed25519.XYZ,
                                            final private[vendored] val t: Array[Long] = new Array[Long](LIMB_CNT)) {

    def this(other: Ed25519.PartialXYZT) = {
      this(new Ed25519.XYZ(other.xyz), util.Arrays.copyOf(other.t, LIMB_CNT))
    }
  }

  /**
   * Corresponds to the caching mentioned in the last paragraph of Section 3.1 of
   * Hisil H., Wong K.KH., Carter G., Dawson E. (2008) Twisted Edwards Curves Revisited.
   * with Z = 1.
   */
  private[vendored] class CachedXYT private[vendored](final private[vendored] val yPlusX: Array[Long] = new Array[Long](LIMB_CNT),
                                                  final private[vendored] val yMinusX: Array[Long] = new Array[Long](LIMB_CNT),
                                                  final private[vendored] val t2d: Array[Long] = new Array[Long](LIMB_CNT)) {
    def this(other: Ed25519.CachedXYT) = {
      this(util.Arrays.copyOf(other.yPlusX, LIMB_CNT), util.Arrays.copyOf(other.yMinusX, LIMB_CNT), util.Arrays.copyOf(other.t2d, LIMB_CNT))
    }

    // z is one implicitly, so this just copies {@code in} to {@code output}.
    private[vendored] def multByZ(output: Array[Long], in: Array[Long]): Unit = {
      System.arraycopy(in, 0, output, 0, LIMB_CNT)
    }

    /**
     * If icopy is 1, copies {@code other} into this point. Time invariant wrt to icopy value.
     */
    private[vendored] def copyConditional(other: Ed25519.CachedXYT, icopy: Int): Unit = {
      Field25519.copyConditional(yPlusX, other.yPlusX, icopy)
      Field25519.copyConditional(yMinusX, other.yMinusX, icopy)
      Field25519.copyConditional(t2d, other.t2d, icopy)
    }
  }

  /**
   * Creates a cached XYZT
   *
   * @param yPlusX  Y + X
   * @param yMinusX Y - X
   * @param z       Z
   * @param t2d     2d * (XY/Z)
   */
  private[vendored] class CachedXYZT private[vendored](yPlusX0: Array[Long] = new Array[Long](LIMB_CNT),
                                           yMinusX0: Array[Long] = new Array[Long](LIMB_CNT),
                                           private val z: Array[Long] = new Array[Long](LIMB_CNT),
                                           t2d0: Array[Long] = new Array[Long](LIMB_CNT),
                                          ) extends Ed25519.CachedXYT(yPlusX0, yMinusX0, t2d0) {

    /**
     * ge_p3_to_cached.c
     */
    def this(xyzt: Ed25519.XYZT) = {
      this()
      Field25519.sum(yPlusX, xyzt.xyz.y, xyzt.xyz.x)
      Field25519.sub(yMinusX, xyzt.xyz.y, xyzt.xyz.x)
      System.arraycopy(xyzt.xyz.z, 0, z, 0, LIMB_CNT)
      Field25519.mult(t2d, xyzt.t, D2)
    }

    override def multByZ(output: Array[Long], in: Array[Long]): Unit = {
      Field25519.mult(output, in, z)
    }
  }

  /**
   * Addition defined in Section 3.1 of
   * Hisil H., Wong K.KH., Carter G., Dawson E. (2008) Twisted Edwards Curves Revisited.
   *
   * Please note that this is a partial of the operation listed there leaving out the final
   * conversion from PartialXYZT to XYZT.
   *
   * @param extended extended projective point input
   * @param cached   cached projective point input
   */
  private def add(partialXYZT: Ed25519.PartialXYZT, extended: Ed25519.XYZT, cached: Ed25519.CachedXYT): Unit = {
    val t = new Array[Long](LIMB_CNT)

    // Y1 + X1
    Field25519.sum(partialXYZT.xyz.x, extended.xyz.y, extended.xyz.x)

    // Y1 - X1
    Field25519.sub(partialXYZT.xyz.y, extended.xyz.y, extended.xyz.x)

    // A = (Y1 - X1) * (Y2 - X2)
    Field25519.mult(partialXYZT.xyz.y, partialXYZT.xyz.y, cached.yMinusX)

    // B = (Y1 + X1) * (Y2 + X2)
    Field25519.mult(partialXYZT.xyz.z, partialXYZT.xyz.x, cached.yPlusX)

    // C = T1 * 2d * T2 = 2d * T1 * T2 (2d is written as k in the paper)
    Field25519.mult(partialXYZT.t, extended.t, cached.t2d)

    // Z1 * Z2
    cached.multByZ(partialXYZT.xyz.x, extended.xyz.z)

    // D = 2 * Z1 * Z2
    Field25519.sum(t, partialXYZT.xyz.x, partialXYZT.xyz.x)

    // X3 = B - A
    Field25519.sub(partialXYZT.xyz.x, partialXYZT.xyz.z, partialXYZT.xyz.y)

    // Y3 = B + A
    Field25519.sum(partialXYZT.xyz.y, partialXYZT.xyz.z, partialXYZT.xyz.y)

    // Z3 = D + C
    Field25519.sum(partialXYZT.xyz.z, t, partialXYZT.t)

    // T3 = D - C
    Field25519.sub(partialXYZT.t, t, partialXYZT.t)
  }

  /**
   * Based on the addition defined in Section 3.1 of
   * Hisil H., Wong K.KH., Carter G., Dawson E. (2008) Twisted Edwards Curves Revisited.
   *
   * Please note that this is a partial of the operation listed there leaving out the final
   * conversion from PartialXYZT to XYZT.
   *
   * @param extended extended projective point input
   * @param cached   cached projective point input
   */
  private def sub(partialXYZT: Ed25519.PartialXYZT, extended: Ed25519.XYZT, cached: Ed25519.CachedXYT): Unit = {
    val t = new Array[Long](LIMB_CNT)

    // Y1 + X1
    Field25519.sum(partialXYZT.xyz.x, extended.xyz.y, extended.xyz.x)

    // Y1 - X1
    Field25519.sub(partialXYZT.xyz.y, extended.xyz.y, extended.xyz.x)

    // A = (Y1 - X1) * (Y2 + X2)
    Field25519.mult(partialXYZT.xyz.y, partialXYZT.xyz.y, cached.yPlusX)

    // B = (Y1 + X1) * (Y2 - X2)
    Field25519.mult(partialXYZT.xyz.z, partialXYZT.xyz.x, cached.yMinusX)

    // C = T1 * 2d * T2 = 2d * T1 * T2 (2d is written as k in the paper)
    Field25519.mult(partialXYZT.t, extended.t, cached.t2d)

    // Z1 * Z2
    cached.multByZ(partialXYZT.xyz.x, extended.xyz.z)

    // D = 2 * Z1 * Z2
    Field25519.sum(t, partialXYZT.xyz.x, partialXYZT.xyz.x)

    // X3 = B - A
    Field25519.sub(partialXYZT.xyz.x, partialXYZT.xyz.z, partialXYZT.xyz.y)

    // Y3 = B + A
    Field25519.sum(partialXYZT.xyz.y, partialXYZT.xyz.z, partialXYZT.xyz.y)

    // Z3 = D - C
    Field25519.sub(partialXYZT.xyz.z, t, partialXYZT.t)

    // T3 = D + C
    Field25519.sum(partialXYZT.t, t, partialXYZT.t)
  }

  /**
   * Doubles {@code p} and puts the result into this PartialXYZT.
   *
   * This is based on the addition defined in formula 7 in Section 3.3 of
   * Hisil H., Wong K.KH., Carter G., Dawson E. (2008) Twisted Edwards Curves Revisited.
   *
   * Please note that this is a partial of the operation listed there leaving out the final
   * conversion from PartialXYZT to XYZT and also this fixes a typo in calculation of Y3 and T3 in
   * the paper, H should be replaced with A+B.
   */
  private def doubleXYZ(partialXYZT: Ed25519.PartialXYZT, p: Ed25519.XYZ): Unit = {
    val t0 = new Array[Long](LIMB_CNT)

    // XX = X1^2
    Field25519.square(partialXYZT.xyz.x, p.x)

    // YY = Y1^2
    Field25519.square(partialXYZT.xyz.z, p.y)

    // B' = Z1^2
    Field25519.square(partialXYZT.t, p.z)

    // B = 2 * B'
    Field25519.sum(partialXYZT.t, partialXYZT.t, partialXYZT.t)

    // A = X1 + Y1
    Field25519.sum(partialXYZT.xyz.y, p.x, p.y)

    // AA = A^2
    Field25519.square(t0, partialXYZT.xyz.y)

    // Y3 = YY + XX
    Field25519.sum(partialXYZT.xyz.y, partialXYZT.xyz.z, partialXYZT.xyz.x)

    // Z3 = YY - XX
    Field25519.sub(partialXYZT.xyz.z, partialXYZT.xyz.z, partialXYZT.xyz.x)

    // X3 = AA - Y3
    Field25519.sub(partialXYZT.xyz.x, t0, partialXYZT.xyz.y)

    // T3 = B - Z3
    Field25519.sub(partialXYZT.t, partialXYZT.t, partialXYZT.xyz.z)
  }

  /**
   * Doubles {@code p} and puts the result into this PartialXYZT.
   */
  private def doubleXYZT(partialXYZT: Ed25519.PartialXYZT, p: Ed25519.XYZT): Unit = {
    doubleXYZ(partialXYZT, p.xyz)
  }

  /**
   * Compares two byte values in constant time.
   *
   * Please note that this doesn't reuse {@link Curve25519.eq} method since the below inputs are
   * byte values.
   */
  private def eq(a: Int, b: Int) = {
    var r = ~(a ^ b) & 0xff
    r &= r << 4
    r &= r << 2
    r &= r << 1
    (r >> 7) & 1
  }

  /**
   * This is a constant time operation where point b*B*256^pos is stored in {@code t}.
   * When b is 0, t remains the same (i.e., neutral point).
   *
   * Although B_TABLE[32][8] (B_TABLE[i][j] = (j+1)*B*256^i) has j values in [0, 7], the select
   * method negates the corresponding point if b is negative (which is straight forward in elliptic
   * curves by just negating y coordinate). Therefore we can get multiples of B with the half of
   * memory requirements.
   *
   * @param t   neutral element (i.e., point 0), also serves as output.
   * @param pos in B[pos][j] = (j+1)*B*256^pos
   * @param b   value in [-8, 8] range.
   */
  private def select(t: Ed25519.CachedXYT, pos: Int, b: Byte): Unit = {
    val bnegative: Int = (b & 0xff) >> 7
    val babs: Int = b - (((-bnegative) & b) << 1)

    t.copyConditional(B_TABLE(pos)(0), eq(babs, 1))
    t.copyConditional(B_TABLE(pos)(1), eq(babs, 2))
    t.copyConditional(B_TABLE(pos)(2), eq(babs, 3))
    t.copyConditional(B_TABLE(pos)(3), eq(babs, 4))
    t.copyConditional(B_TABLE(pos)(4), eq(babs, 5))
    t.copyConditional(B_TABLE(pos)(5), eq(babs, 6))
    t.copyConditional(B_TABLE(pos)(6), eq(babs, 7))
    t.copyConditional(B_TABLE(pos)(7), eq(babs, 8))

    val yPlusX = util.Arrays.copyOf(t.yMinusX, LIMB_CNT)
    val yMinusX = util.Arrays.copyOf(t.yPlusX, LIMB_CNT)
    val t2d = util.Arrays.copyOf(t.t2d, LIMB_CNT)
    neg(t2d, t2d)
    val minust = new Ed25519.CachedXYT(yPlusX, yMinusX, t2d)
    t.copyConditional(minust, bnegative)
  }

  /**
   * Computes {@code a}*B
   * where a = a[0]+256*a[1]+...+256^31 a[31] and
   * B is the Ed25519 base point (x,4/5) with x positive.
   *
   * Preconditions:
   * a[31] <= 127
   *
   * @throws IllegalStateException iff there is arithmetic error.
   */
  @SuppressWarnings(Array("NarrowingCompoundAssignment"))
  private def scalarMultWithBase(a: Array[Byte]) = {
    val e = new Array[Byte](2 * FIELD_LEN);
    {
      var i = 0
      while i < FIELD_LEN do {
        e(2 * i + 0) = (((a(i) & 0xff) >> 0) & 0xf).toByte
        e(2 * i + 1) = (((a(i) & 0xff) >> 4) & 0xf).toByte
        i += 1
      }
    }
    // each e[i] is between 0 and 15
    // e[63] is between 0 and 7

    // Rewrite e in a way that each e[i] is in [-8, 8].
    // This can be done since a[63] is in [0, 7], the carry-over onto the most significant byte
    // a[63] can be at most 1.
    var carry = 0
    for i <- 0 until e.length - 1 do {
      e(i) = (e(i) + carry).toByte
      carry = e(i) + 8
      carry >>= 4
      e(i) = (e(i) - (carry << 4)).toByte
    }
    e(e.length - 1) = (e(e.length - 1) + carry).toByte

    val ret: PartialXYZT = new Ed25519.PartialXYZT(NEUTRAL)
    val xyzt: XYZT = new Ed25519.XYZT();
    // Although B_TABLE's i can be at most 31 (stores only 32 4bit multiples of B) and we have 64
    // 4bit values in e array, the below for loop adds cached values by iterating e by two in odd
    // indices. After the result, we can double the result point 4 times to shift the multiplication
    // scalar by 4 bits.
    {
      var i = 1
      while i < e.length do {
        val t = new Ed25519.CachedXYT(CACHED_NEUTRAL)
        select(t, i / 2, e(i))
        add(ret, XYZT.fromPartialXYZT(xyzt, ret), t)
        i += 2
      }
    }

    // Doubles the result 4 times to shift the multiplication scalar 4 bits to get the actual result
    // for the odd indices in e.
    val xyz = new Ed25519.XYZ
    doubleXYZ(ret, XYZ.fromPartialXYZT(xyz, ret))
    doubleXYZ(ret, XYZ.fromPartialXYZT(xyz, ret))
    doubleXYZ(ret, XYZ.fromPartialXYZT(xyz, ret))
    doubleXYZ(ret, XYZ.fromPartialXYZT(xyz, ret))

    // Add multiples of B for even indices of e.
    {
      var i = 0
      while i < e.length do {
        val t = new Ed25519.CachedXYT(CACHED_NEUTRAL)
        select(t, i / 2, e(i))
        add(ret, XYZT.fromPartialXYZT(xyzt, ret), t)
        i += 2
      }
    }

    // This check is to protect against flaws, i.e. if there is a computation error through a
    // faulty CPU or if the implementation contains a bug.
    val result = new Ed25519.XYZ(ret)
    if !result.isOnCurve then {
      throw new IllegalStateException("arithmetic error in scalar multiplication")
    }
    result
  }

  /**
   * Computes {@code a}*B
   * where a = a[0]+256*a[1]+...+256^31 a[31] and
   * B is the Ed25519 base point (x,4/5) with x positive.
   *
   * Preconditions:
   * a[31] <= 127
   */
  private[vendored] def scalarMultWithBaseToBytes(a: Array[Byte]) = scalarMultWithBase(a).toBytes

  @SuppressWarnings(Array("NarrowingCompoundAssignment"))
  private def slide(a: Array[Byte]) = {
    val r = new Array[Byte](256)
    // Writes each bit in a[0..31] into r[0..255]:
    // a = a[0]+256*a[1]+...+256^31*a[31] is equal to
    // r = r[0]+2*r[1]+...+2^255*r[255]
    for i <- 0 until 256 do {
      r(i) = (1 & ((a(i >> 3) & 0xff) >> (i & 7))).toByte
    }

    // Transforms r[i] as odd values in [-15, 15]
    for i <- 0 until 256 do {
      if r(i) != 0 then {
        var b = 1
        while b <= 6 && i + b < 256 && {
          if r(i + b) != 0 then {
            if r(i) + (r(i + b) << b) <= 15 then {
              r(i) = (r(i) + (r(i + b) << b)).toByte
              r(i + b) = 0
              true
            } else if r(i) - (r(i + b) << b) >= -15 then {
              r(i) = (r(i) - (r(i + b) << b)).toByte
              {
                var k: Int = i + b
                while k < 256 && {
                  if r(k) == 0 then {
                    r(k) = 1
                    false
                  } else {
                    r(k) = 0
                    true
                  }
                } do {
                  k += 1
                }
              }
              true
            } else {
              false
            }
          } else {
            true
          }
        } do {b += 1}
      }
    }
    r
  }

  /**
   * Computes {@code a}*{@code pointA}+{@code b}*B
   * where a = a[0]+256*a[1]+...+256^31*a[31].
   * and b = b[0]+256*b[1]+...+256^31*b[31].
   * B is the Ed25519 base point (x,4/5) with x positive.
   *
   * Note that execution time varies based on the input since this will only be used in verification
   * of signatures.
   */
  private def doubleScalarMultVarTime(a: Array[Byte], pointA: Ed25519.XYZT, b: Array[Byte]) = {
    // pointA, 3*pointA, 5*pointA, 7*pointA, 9*pointA, 11*pointA, 13*pointA, 15*pointA
    val pointAArray = new Array[Ed25519.CachedXYZT](8)
    pointAArray(0) = new Ed25519.CachedXYZT(pointA)
    var t = new Ed25519.PartialXYZT
    doubleXYZT(t, pointA)
    val doubleA = new Ed25519.XYZT(t)
    for i <- 1 until pointAArray.length do {
      add(t, doubleA, pointAArray(i - 1))
      pointAArray(i) = new Ed25519.CachedXYZT(new Ed25519.XYZT(t))
    }
    val aSlide = slide(a)
    val bSlide = slide(b)
    t = new Ed25519.PartialXYZT(NEUTRAL)
    val u = new Ed25519.XYZT
    var i = 255
    while i >= 0 && !(aSlide(i) != 0 || bSlide(i) != 0) do {
      i -= 1
    }
    while i >= 0 do {
      doubleXYZ(t, new Ed25519.XYZ(t))
      if aSlide(i) > 0 then {
        add(t, XYZT.fromPartialXYZT(u, t), pointAArray(aSlide(i) / 2))
      } else if aSlide(i) < 0 then {
        sub(t, XYZT.fromPartialXYZT(u, t), pointAArray(-aSlide(i) / 2))
      }
      if bSlide(i) > 0 then {
        add(t, XYZT.fromPartialXYZT(u, t), B2(bSlide(i) / 2))
      } else if bSlide(i) < 0 then {
        sub(t, XYZT.fromPartialXYZT(u, t), B2(-bSlide(i) / 2))
      }
      i -= 1
    }

    new Ed25519.XYZ(t)
  }

  /**
   * Returns true if {@code in} is nonzero.
   *
   * Note that execution time might depend on the input {@code in}.
   */
  private def isNonZeroVarTime(in: Array[Long]): Boolean = {
    val inCopy = new Array[Long](in.length + 1)
    System.arraycopy(in, 0, inCopy, 0, in.length)
    Field25519.reduceCoefficients(inCopy)
    val bytes = Field25519.contract(inCopy)

    {
      var i = 0
      while i < bytes.length do
        if bytes(i) != 0 then {
          return true
        }
        i += 1
    }
    false
  }

  /**
   * Returns the least significant bit of {@code in}.
   */
  private def getLsb(in: Array[Long]) = Field25519.contract(in)(0) & 1

  /**
   * Negates all values in {@code in} and store it in {@code out}.
   */
  private def neg(out: Array[Long], in: Array[Long]): Unit = {
    for i <- 0 until in.length do {
      out(i) = -in(i)
    }
  }

  /**
   * Computes {@code in}^(2^252-3) mod 2^255-19 and puts the result in {@code out}.
   */
  private def pow2252m3(out: Array[Long], in: Array[Long]): Unit = {
    val t0 = new Array[Long](LIMB_CNT)
    val t1 = new Array[Long](LIMB_CNT)
    val t2 = new Array[Long](LIMB_CNT)

    // z2 = z1^2^1
    Field25519.square(t0, in)

    // z8 = z2^2^2
    Field25519.square(t1, t0)
    for i <- 1 until 2 do {
      Field25519.square(t1, t1)
    }

    // z9 = z1*z8
    Field25519.mult(t1, in, t1)

    // z11 = z2*z9
    Field25519.mult(t0, t0, t1)

    // z22 = z11^2^1
    Field25519.square(t0, t0)

    // z_5_0 = z9*z22
    Field25519.mult(t0, t1, t0)

    // z_10_5 = z_5_0^2^5
    Field25519.square(t1, t0)
    for i <- 1 until 5 do {
      Field25519.square(t1, t1)
    }

    // z_10_0 = z_10_5*z_5_0
    Field25519.mult(t0, t1, t0)

    // z_20_10 = z_10_0^2^10
    Field25519.square(t1, t0)
    for i <- 1 until 10 do {
      Field25519.square(t1, t1)
    }

    // z_20_0 = z_20_10*z_10_0
    Field25519.mult(t1, t1, t0)

    // z_40_20 = z_20_0^2^20
    Field25519.square(t2, t1)
    for i <- 1 until 20 do {
      Field25519.square(t2, t2)
    }

    // z_40_0 = z_40_20*z_20_0
    Field25519.mult(t1, t2, t1)

    // z_50_10 = z_40_0^2^10
    Field25519.square(t1, t1)
    for i <- 1 until 10 do {
      Field25519.square(t1, t1)
    }

    // z_50_0 = z_50_10*z_10_0
    Field25519.mult(t0, t1, t0)

    // z_100_50 = z_50_0^2^50
    Field25519.square(t1, t0)
    for i <- 1 until 50 do {
      Field25519.square(t1, t1)
    }

    // z_100_0 = z_100_50*z_50_0
    Field25519.mult(t1, t1, t0)

    // z_200_100 = z_100_0^2^100
    Field25519.square(t2, t1)
    for i <- 1 until 100 do {
      Field25519.square(t2, t2)
    }

    // z_200_0 = z_200_100*z_100_0
    Field25519.mult(t1, t2, t1)

    // z_250_50 = z_200_0^2^50
    Field25519.square(t1, t1)
    for i <- 1 until 50 do {
      Field25519.square(t1, t1)
    }

    // z_250_0 = z_250_50*z_50_0
    Field25519.mult(t0, t1, t0)

    // z_252_2 = z_250_0^2^2
    Field25519.square(t0, t0)
    for i <- 1 until 2 do {
      Field25519.square(t0, t0)
    }

    // z_252_3 = z_252_2*z1
    Field25519.mult(out, t0, in)
  }

  /**
   * Returns 3 bytes of {@code in} starting from {@code idx} in Little-Endian format.
   */
  private def load3(in: Array[Byte], idx: Int) = {
    var result = 0L
    result = in(idx).toLong & 0xff
    result |= (in(idx + 1) & 0xff).toLong << 8
    result |= (in(idx + 2) & 0xff).toLong << 16
    result
  }

  /**
   * Returns 4 bytes of {@code in} starting from {@code idx} in Little-Endian format.
   */
  private def load4(in: Array[Byte], idx: Int) = {
    var result = load3(in, idx)
    result |= (in(idx + 3) & 0xff).toLong << 24
    result
  }

  /**
   * Input:
   * s[0]+256*s[1]+...+256^63*s[63] = s
   *
   * Output:
   * s[0]+256*s[1]+...+256^31*s[31] = s mod l
   * where l = 2^252 + 27742317777372353535851937790883648493.
   * Overwrites s in place.
   */
  private def reduce(s: Array[Byte]): Unit = {
    // Observation:
    // 2^252 mod l is equivalent to -27742317777372353535851937790883648493 mod l
    // Let m = -27742317777372353535851937790883648493
    // Thus a*2^252+b mod l is equivalent to a*m+b mod l
    //
    // First s is divided into chunks of 21 bits as follows:
    // s0+2^21*s1+2^42*s3+...+2^462*s23 = s[0]+256*s[1]+...+256^63*s[63]
    var s0 = 2097151 & load3(s, 0)
    var s1 = 2097151 & (load4(s, 2) >> 5)
    var s2 = 2097151 & (load3(s, 5) >> 2)
    var s3 = 2097151 & (load4(s, 7) >> 7)
    var s4 = 2097151 & (load4(s, 10) >> 4)
    var s5 = 2097151 & (load3(s, 13) >> 1)
    var s6 = 2097151 & (load4(s, 15) >> 6)
    var s7 = 2097151 & (load3(s, 18) >> 3)
    var s8 = 2097151 & load3(s, 21)
    var s9 = 2097151 & (load4(s, 23) >> 5)
    var s10 = 2097151 & (load3(s, 26) >> 2)
    var s11 = 2097151 & (load4(s, 28) >> 7)
    var s12 = 2097151 & (load4(s, 31) >> 4)
    var s13 = 2097151 & (load3(s, 34) >> 1)
    var s14 = 2097151 & (load4(s, 36) >> 6)
    var s15 = 2097151 & (load3(s, 39) >> 3)
    var s16 = 2097151 & load3(s, 42)
    var s17 = 2097151 & (load4(s, 44) >> 5)
    val s18 = 2097151 & (load3(s, 47) >> 2)
    val s19 = 2097151 & (load4(s, 49) >> 7)
    val s20 = 2097151 & (load4(s, 52) >> 4)
    val s21 = 2097151 & (load3(s, 55) >> 1)
    val s22 = 2097151 & (load4(s, 57) >> 6)
    val s23 = load4(s, 60) >> 3
    var carry0 = 0L
    var carry1 = 0L
    var carry2 = 0L
    var carry3 = 0L
    var carry4 = 0L
    var carry5 = 0L
    var carry6 = 0L
    var carry7 = 0L
    var carry8 = 0L
    var carry9 = 0L
    var carry10 = 0L
    var carry11 = 0L
    var carry12 = 0L
    var carry13 = 0L
    var carry14 = 0L
    var carry15 = 0L
    var carry16 = 0L

    // s23*2^462 = s23*2^210*2^252 is equivalent to s23*2^210*m in mod l
    // As m is a 125 bit number, the result needs to scattered to 6 limbs (125/21 ceil is 6)
    // starting from s11 (s11*2^210)
    // m = [666643, 470296, 654183, -997805, 136657, -683901] in 21-bit limbs
    s11 += s23 * 666643
    s12 += s23 * 470296
    s13 += s23 * 654183
    s14 -= s23 * 997805
    s15 += s23 * 136657
    s16 -= s23 * 683901
    // s23 = 0;

    s10 += s22 * 666643
    s11 += s22 * 470296
    s12 += s22 * 654183
    s13 -= s22 * 997805
    s14 += s22 * 136657
    s15 -= s22 * 683901
    // s22 = 0;

    s9 += s21 * 666643
    s10 += s21 * 470296
    s11 += s21 * 654183
    s12 -= s21 * 997805
    s13 += s21 * 136657
    s14 -= s21 * 683901
    // s21 = 0;

    s8 += s20 * 666643
    s9 += s20 * 470296
    s10 += s20 * 654183
    s11 -= s20 * 997805
    s12 += s20 * 136657
    s13 -= s20 * 683901
    // s20 = 0;

    s7 += s19 * 666643
    s8 += s19 * 470296
    s9 += s19 * 654183
    s10 -= s19 * 997805
    s11 += s19 * 136657
    s12 -= s19 * 683901
    // s19 = 0;

    s6 += s18 * 666643
    s7 += s18 * 470296
    s8 += s18 * 654183
    s9 -= s18 * 997805
    s10 += s18 * 136657
    s11 -= s18 * 683901
    // s18 = 0;

    // Reduce the bit length of limbs from s6 to s15 to 21-bits.
    carry6 = (s6 + (1 << 20)) >> 21; s7 += carry6; s6 -= carry6 << 21
    carry8 = (s8 + (1 << 20)) >> 21; s9 += carry8; s8 -= carry8 << 21
    carry10 = (s10 + (1 << 20)) >> 21; s11 += carry10; s10 -= carry10 << 21
    carry12 = (s12 + (1 << 20)) >> 21; s13 += carry12; s12 -= carry12 << 21
    carry14 = (s14 + (1 << 20)) >> 21; s15 += carry14; s14 -= carry14 << 21
    carry16 = (s16 + (1 << 20)) >> 21; s17 += carry16; s16 -= carry16 << 21

    carry7 = (s7 + (1 << 20)) >> 21; s8 += carry7; s7 -= carry7 << 21
    carry9 = (s9 + (1 << 20)) >> 21; s10 += carry9; s9 -= carry9 << 21
    carry11 = (s11 + (1 << 20)) >> 21; s12 += carry11; s11 -= carry11 << 21
    carry13 = (s13 + (1 << 20)) >> 21; s14 += carry13; s13 -= carry13 << 21
    carry15 = (s15 + (1 << 20)) >> 21; s16 += carry15; s15 -= carry15 << 21

    // Resume reduction where we left off.
    s5 += s17 * 666643
    s6 += s17 * 470296
    s7 += s17 * 654183
    s8 -= s17 * 997805
    s9 += s17 * 136657
    s10 -= s17 * 683901
    // s17 = 0;

    s4 += s16 * 666643
    s5 += s16 * 470296
    s6 += s16 * 654183
    s7 -= s16 * 997805
    s8 += s16 * 136657
    s9 -= s16 * 683901
    // s16 = 0;

    s3 += s15 * 666643
    s4 += s15 * 470296
    s5 += s15 * 654183
    s6 -= s15 * 997805
    s7 += s15 * 136657
    s8 -= s15 * 683901
    // s15 = 0;

    s2 += s14 * 666643
    s3 += s14 * 470296
    s4 += s14 * 654183
    s5 -= s14 * 997805
    s6 += s14 * 136657
    s7 -= s14 * 683901
    // s14 = 0;

    s1 += s13 * 666643
    s2 += s13 * 470296
    s3 += s13 * 654183
    s4 -= s13 * 997805
    s5 += s13 * 136657
    s6 -= s13 * 683901
    // s13 = 0;

    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    s12 = 0

    // Reduce the range of limbs from s0 to s11 to 21-bits.
    carry0 = (s0 + (1 << 20)) >> 21; s1 += carry0; s0 -= carry0 << 21
    carry2 = (s2 + (1 << 20)) >> 21; s3 += carry2; s2 -= carry2 << 21
    carry4 = (s4 + (1 << 20)) >> 21; s5 += carry4; s4 -= carry4 << 21
    carry6 = (s6 + (1 << 20)) >> 21; s7 += carry6; s6 -= carry6 << 21
    carry8 = (s8 + (1 << 20)) >> 21; s9 += carry8; s8 -= carry8 << 21
    carry10 = (s10 + (1 << 20)) >> 21; s11 += carry10; s10 -= carry10 << 21

    carry1 = (s1 + (1 << 20)) >> 21; s2 += carry1; s1 -= carry1 << 21
    carry3 = (s3 + (1 << 20)) >> 21; s4 += carry3; s3 -= carry3 << 21
    carry5 = (s5 + (1 << 20)) >> 21; s6 += carry5; s5 -= carry5 << 21
    carry7 = (s7 + (1 << 20)) >> 21; s8 += carry7; s7 -= carry7 << 21
    carry9 = (s9 + (1 << 20)) >> 21; s10 += carry9; s9 -= carry9 << 21
    carry11 = (s11 + (1 << 20)) >> 21; s12 += carry11; s11 -= carry11 << 21

    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    s12 = 0

    // Carry chain reduction to propagate excess bits from s0 to s5 to the most significant limbs.
    carry0 = s0 >> 21; s1 += carry0; s0 -= carry0 << 21
    carry1 = s1 >> 21; s2 += carry1; s1 -= carry1 << 21
    carry2 = s2 >> 21; s3 += carry2; s2 -= carry2 << 21
    carry3 = s3 >> 21; s4 += carry3; s3 -= carry3 << 21
    carry4 = s4 >> 21; s5 += carry4; s4 -= carry4 << 21
    carry5 = s5 >> 21; s6 += carry5; s5 -= carry5 << 21
    carry6 = s6 >> 21; s7 += carry6; s6 -= carry6 << 21
    carry7 = s7 >> 21; s8 += carry7; s7 -= carry7 << 21
    carry8 = s8 >> 21; s9 += carry8; s8 -= carry8 << 21
    carry9 = s9 >> 21; s10 += carry9; s9 -= carry9 << 21
    carry10 = s10 >> 21; s11 += carry10; s10 -= carry10 << 21
    carry11 = s11 >> 21; s12 += carry11; s11 -= carry11 << 21

    // Do one last reduction as s12 might be 1.
    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    // s12 = 0;

    carry0 = s0 >> 21; s1 += carry0; s0 -= carry0 << 21
    carry1 = s1 >> 21; s2 += carry1; s1 -= carry1 << 21
    carry2 = s2 >> 21; s3 += carry2; s2 -= carry2 << 21
    carry3 = s3 >> 21; s4 += carry3; s3 -= carry3 << 21
    carry4 = s4 >> 21; s5 += carry4; s4 -= carry4 << 21
    carry5 = s5 >> 21; s6 += carry5; s5 -= carry5 << 21
    carry6 = s6 >> 21; s7 += carry6; s6 -= carry6 << 21
    carry7 = s7 >> 21; s8 += carry7; s7 -= carry7 << 21
    carry8 = s8 >> 21; s9 += carry8; s8 -= carry8 << 21
    carry9 = s9 >> 21; s10 += carry9; s9 -= carry9 << 21
    carry10 = s10 >> 21; s11 += carry10; s10 -= carry10 << 21

    // Serialize the result into the s.
    s(0) = s0.toByte
    s(1) = (s0 >> 8).toByte
    s(2) = ((s0 >> 16) | (s1 << 5)).toByte
    s(3) = (s1 >> 3).toByte
    s(4) = (s1 >> 11).toByte
    s(5) = ((s1 >> 19) | (s2 << 2)).toByte
    s(6) = (s2 >> 6).toByte
    s(7) = ((s2 >> 14) | (s3 << 7)).toByte
    s(8) = (s3 >> 1).toByte
    s(9) = (s3 >> 9).toByte
    s(10) = ((s3 >> 17) | (s4 << 4)).toByte
    s(11) = (s4 >> 4).toByte
    s(12) = (s4 >> 12).toByte
    s(13) = ((s4 >> 20) | (s5 << 1)).toByte
    s(14) = (s5 >> 7).toByte
    s(15) = ((s5 >> 15) | (s6 << 6)).toByte
    s(16) = (s6 >> 2).toByte
    s(17) = (s6 >> 10).toByte
    s(18) = ((s6 >> 18) | (s7 << 3)).toByte
    s(19) = (s7 >> 5).toByte
    s(20) = (s7 >> 13).toByte
    s(21) = s8.toByte
    s(22) = (s8 >> 8).toByte
    s(23) = ((s8 >> 16) | (s9 << 5)).toByte
    s(24) = (s9 >> 3).toByte
    s(25) = (s9 >> 11).toByte
    s(26) = ((s9 >> 19) | (s10 << 2)).toByte
    s(27) = (s10 >> 6).toByte
    s(28) = ((s10 >> 14) | (s11 << 7)).toByte
    s(29) = (s11 >> 1).toByte
    s(30) = (s11 >> 9).toByte
    s(31) = (s11 >> 17).toByte
  }

  /**
   * Input:
   * a[0]+256*a[1]+...+256^31*a[31] = a
   * b[0]+256*b[1]+...+256^31*b[31] = b
   * c[0]+256*c[1]+...+256^31*c[31] = c
   *
   * Output:
   * s[0]+256*s[1]+...+256^31*s[31] = (ab+c) mod l
   * where l = 2^252 + 27742317777372353535851937790883648493.
   */
  private def mulAdd(s: Array[Byte], a: Array[Byte], b: Array[Byte], c: Array[Byte]): Unit = {
    // This is very similar to Ed25519.reduce, the difference in here is that it computes ab+c
    // See Ed25519.reduce for related comments.
    val a0 = 2097151 & load3(a, 0)
    val a1 = 2097151 & (load4(a, 2) >> 5)
    val a2 = 2097151 & (load3(a, 5) >> 2)
    val a3 = 2097151 & (load4(a, 7) >> 7)
    val a4 = 2097151 & (load4(a, 10) >> 4)
    val a5 = 2097151 & (load3(a, 13) >> 1)
    val a6 = 2097151 & (load4(a, 15) >> 6)
    val a7 = 2097151 & (load3(a, 18) >> 3)
    val a8 = 2097151 & load3(a, 21)
    val a9 = 2097151 & (load4(a, 23) >> 5)
    val a10 = 2097151 & (load3(a, 26) >> 2)
    val a11 = load4(a, 28) >> 7
    val b0 = 2097151 & load3(b, 0)
    val b1 = 2097151 & (load4(b, 2) >> 5)
    val b2 = 2097151 & (load3(b, 5) >> 2)
    val b3 = 2097151 & (load4(b, 7) >> 7)
    val b4 = 2097151 & (load4(b, 10) >> 4)
    val b5 = 2097151 & (load3(b, 13) >> 1)
    val b6 = 2097151 & (load4(b, 15) >> 6)
    val b7 = 2097151 & (load3(b, 18) >> 3)
    val b8 = 2097151 & load3(b, 21)
    val b9 = 2097151 & (load4(b, 23) >> 5)
    val b10 = 2097151 & (load3(b, 26) >> 2)
    val b11 = load4(b, 28) >> 7
    val c0 = 2097151 & load3(c, 0)
    val c1 = 2097151 & (load4(c, 2) >> 5)
    val c2 = 2097151 & (load3(c, 5) >> 2)
    val c3 = 2097151 & (load4(c, 7) >> 7)
    val c4 = 2097151 & (load4(c, 10) >> 4)
    val c5 = 2097151 & (load3(c, 13) >> 1)
    val c6 = 2097151 & (load4(c, 15) >> 6)
    val c7 = 2097151 & (load3(c, 18) >> 3)
    val c8 = 2097151 & load3(c, 21)
    val c9 = 2097151 & (load4(c, 23) >> 5)
    val c10 = 2097151 & (load3(c, 26) >> 2)
    val c11 = load4(c, 28) >> 7
    var s0 = 0L
    var s1 = 0L
    var s2 = 0L
    var s3 = 0L
    var s4 = 0L
    var s5 = 0L
    var s6 = 0L
    var s7 = 0L
    var s8 = 0L
    var s9 = 0L
    var s10 = 0L
    var s11 = 0L
    var s12 = 0L
    var s13 = 0L
    var s14 = 0L
    var s15 = 0L
    var s16 = 0L
    var s17 = 0L
    var s18 = 0L
    var s19 = 0L
    var s20 = 0L
    var s21 = 0L
    var s22 = 0L
    var s23 = 0L
    var carry0 = 0L
    var carry1 = 0L
    var carry2 = 0L
    var carry3 = 0L
    var carry4 = 0L
    var carry5 = 0L
    var carry6 = 0L
    var carry7 = 0L
    var carry8 = 0L
    var carry9 = 0L
    var carry10 = 0L
    var carry11 = 0L
    var carry12 = 0L
    var carry13 = 0L
    var carry14 = 0L
    var carry15 = 0L
    var carry16 = 0L
    var carry17 = 0L
    var carry18 = 0L
    var carry19 = 0L
    var carry20 = 0L
    var carry21 = 0L
    var carry22 = 0L

    s0 = c0 + a0 * b0
    s1 = c1 + a0 * b1 + a1 * b0
    s2 = c2 + a0 * b2 + a1 * b1 + a2 * b0
    s3 = c3 + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
    s4 = c4 + a0 * b4 + a1 * b3 + a2 * b2 + a3 * b1 + a4 * b0
    s5 = c5 + a0 * b5 + a1 * b4 + a2 * b3 + a3 * b2 + a4 * b1 + a5 * b0
    s6 = c6 + a0 * b6 + a1 * b5 + a2 * b4 + a3 * b3 + a4 * b2 + a5 * b1 + a6 * b0
    s7 = c7 + a0 * b7 + a1 * b6 + a2 * b5 + a3 * b4 + a4 * b3 + a5 * b2 + a6 * b1 + a7 * b0
    s8 = c8 + a0 * b8 + a1 * b7 + a2 * b6 + a3 * b5 + a4 * b4 + a5 * b3 + a6 * b2 + a7 * b1
        + a8 * b0
    s9 = c9 + a0 * b9 + a1 * b8 + a2 * b7 + a3 * b6 + a4 * b5 + a5 * b4 + a6 * b3 + a7 * b2
        + a8 * b1 + a9 * b0
    s10 = c10 + a0 * b10 + a1 * b9 + a2 * b8 + a3 * b7 + a4 * b6 + a5 * b5 + a6 * b4 + a7 * b3
        + a8 * b2 + a9 * b1 + a10 * b0
    s11 = c11 + a0 * b11 + a1 * b10 + a2 * b9 + a3 * b8 + a4 * b7 + a5 * b6 + a6 * b5 + a7 * b4
        + a8 * b3 + a9 * b2 + a10 * b1 + a11 * b0
    s12 = a1 * b11 + a2 * b10 + a3 * b9 + a4 * b8 + a5 * b7 + a6 * b6 + a7 * b5 + a8 * b4 + a9 * b3
        + a10 * b2 + a11 * b1
    s13 = a2 * b11 + a3 * b10 + a4 * b9 + a5 * b8 + a6 * b7 + a7 * b6 + a8 * b5 + a9 * b4 + a10 * b3
        + a11 * b2
    s14 = a3 * b11 + a4 * b10 + a5 * b9 + a6 * b8 + a7 * b7 + a8 * b6 + a9 * b5 + a10 * b4
        + a11 * b3
    s15 = a4 * b11 + a5 * b10 + a6 * b9 + a7 * b8 + a8 * b7 + a9 * b6 + a10 * b5 + a11 * b4
    s16 = a5 * b11 + a6 * b10 + a7 * b9 + a8 * b8 + a9 * b7 + a10 * b6 + a11 * b5
    s17 = a6 * b11 + a7 * b10 + a8 * b9 + a9 * b8 + a10 * b7 + a11 * b6
    s18 = a7 * b11 + a8 * b10 + a9 * b9 + a10 * b8 + a11 * b7
    s19 = a8 * b11 + a9 * b10 + a10 * b9 + a11 * b8
    s20 = a9 * b11 + a10 * b10 + a11 * b9
    s21 = a10 * b11 + a11 * b10
    s22 = a11 * b11
    s23 = 0

    carry0 = (s0 + (1 << 20)) >> 21; s1 += carry0; s0 -= carry0 << 21
    carry2 = (s2 + (1 << 20)) >> 21; s3 += carry2; s2 -= carry2 << 21
    carry4 = (s4 + (1 << 20)) >> 21; s5 += carry4; s4 -= carry4 << 21
    carry6 = (s6 + (1 << 20)) >> 21; s7 += carry6; s6 -= carry6 << 21
    carry8 = (s8 + (1 << 20)) >> 21; s9 += carry8; s8 -= carry8 << 21
    carry10 = (s10 + (1 << 20)) >> 21; s11 += carry10; s10 -= carry10 << 21
    carry12 = (s12 + (1 << 20)) >> 21; s13 += carry12; s12 -= carry12 << 21
    carry14 = (s14 + (1 << 20)) >> 21; s15 += carry14; s14 -= carry14 << 21
    carry16 = (s16 + (1 << 20)) >> 21; s17 += carry16; s16 -= carry16 << 21
    carry18 = (s18 + (1 << 20)) >> 21; s19 += carry18; s18 -= carry18 << 21
    carry20 = (s20 + (1 << 20)) >> 21; s21 += carry20; s20 -= carry20 << 21
    carry22 = (s22 + (1 << 20)) >> 21; s23 += carry22; s22 -= carry22 << 21

    carry1 = (s1 + (1 << 20)) >> 21; s2 += carry1; s1 -= carry1 << 21
    carry3 = (s3 + (1 << 20)) >> 21; s4 += carry3; s3 -= carry3 << 21
    carry5 = (s5 + (1 << 20)) >> 21; s6 += carry5; s5 -= carry5 << 21
    carry7 = (s7 + (1 << 20)) >> 21; s8 += carry7; s7 -= carry7 << 21
    carry9 = (s9 + (1 << 20)) >> 21; s10 += carry9; s9 -= carry9 << 21
    carry11 = (s11 + (1 << 20)) >> 21; s12 += carry11; s11 -= carry11 << 21
    carry13 = (s13 + (1 << 20)) >> 21; s14 += carry13; s13 -= carry13 << 21
    carry15 = (s15 + (1 << 20)) >> 21; s16 += carry15; s15 -= carry15 << 21
    carry17 = (s17 + (1 << 20)) >> 21; s18 += carry17; s17 -= carry17 << 21
    carry19 = (s19 + (1 << 20)) >> 21; s20 += carry19; s19 -= carry19 << 21;
    carry21 = (s21 + (1 << 20)) >> 21; s22 += carry21; s21 -= carry21 << 21

    s11 += s23 * 666643
    s12 += s23 * 470296
    s13 += s23 * 654183
    s14 -= s23 * 997805
    s15 += s23 * 136657
    s16 -= s23 * 683901
    // s23 = 0;

    s10 += s22 * 666643
    s11 += s22 * 470296
    s12 += s22 * 654183
    s13 -= s22 * 997805
    s14 += s22 * 136657
    s15 -= s22 * 683901
    // s22 = 0;

    s9 += s21 * 666643
    s10 += s21 * 470296
    s11 += s21 * 654183
    s12 -= s21 * 997805
    s13 += s21 * 136657
    s14 -= s21 * 683901
    // s21 = 0;

    s8 += s20 * 666643
    s9 += s20 * 470296
    s10 += s20 * 654183
    s11 -= s20 * 997805
    s12 += s20 * 136657
    s13 -= s20 * 683901
    // s20 = 0;

    s7 += s19 * 666643
    s8 += s19 * 470296
    s9 += s19 * 654183
    s10 -= s19 * 997805
    s11 += s19 * 136657
    s12 -= s19 * 683901
    // s19 = 0;

    s6 += s18 * 666643
    s7 += s18 * 470296
    s8 += s18 * 654183
    s9 -= s18 * 997805
    s10 += s18 * 136657
    s11 -= s18 * 683901
    // s18 = 0;

    carry6 = (s6 + (1 << 20)) >> 21; s7 += carry6; s6 -= carry6 << 21
    carry8 = (s8 + (1 << 20)) >> 21; s9 += carry8; s8 -= carry8 << 21
    carry10 = (s10 + (1 << 20)) >> 21; s11 += carry10; s10 -= carry10 << 21
    carry12 = (s12 + (1 << 20)) >> 21; s13 += carry12; s12 -= carry12 << 21
    carry14 = (s14 + (1 << 20)) >> 21; s15 += carry14; s14 -= carry14 << 21
    carry16 = (s16 + (1 << 20)) >> 21; s17 += carry16; s16 -= carry16 << 21

    carry7 = (s7 + (1 << 20)) >> 21; s8 += carry7; s7 -= carry7 << 21
    carry9 = (s9 + (1 << 20)) >> 21; s10 += carry9; s9 -= carry9 << 21
    carry11 = (s11 + (1 << 20)) >> 21; s12 += carry11; s11 -= carry11 << 21
    carry13 = (s13 + (1 << 20)) >> 21; s14 += carry13; s13 -= carry13 << 21
    carry15 = (s15 + (1 << 20)) >> 21; s16 += carry15; s15 -= carry15 << 21

    s5 += s17 * 666643
    s6 += s17 * 470296
    s7 += s17 * 654183
    s8 -= s17 * 997805
    s9 += s17 * 136657
    s10 -= s17 * 683901
    // s17 = 0;

    s4 += s16 * 666643
    s5 += s16 * 470296
    s6 += s16 * 654183
    s7 -= s16 * 997805
    s8 += s16 * 136657
    s9 -= s16 * 683901
    // s16 = 0;

    s3 += s15 * 666643
    s4 += s15 * 470296
    s5 += s15 * 654183
    s6 -= s15 * 997805
    s7 += s15 * 136657
    s8 -= s15 * 683901
    // s15 = 0;

    s2 += s14 * 666643
    s3 += s14 * 470296
    s4 += s14 * 654183
    s5 -= s14 * 997805
    s6 += s14 * 136657
    s7 -= s14 * 683901
    // s14 = 0;

    s1 += s13 * 666643
    s2 += s13 * 470296
    s3 += s13 * 654183
    s4 -= s13 * 997805
    s5 += s13 * 136657
    s6 -= s13 * 683901
    // s13 = 0;

    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    s12 = 0

    carry0 = (s0 + (1 << 20)) >> 21; s1 += carry0; s0 -= carry0 << 21
    carry2 = (s2 + (1 << 20)) >> 21; s3 += carry2; s2 -= carry2 << 21
    carry4 = (s4 + (1 << 20)) >> 21; s5 += carry4; s4 -= carry4 << 21
    carry6 = (s6 + (1 << 20)) >> 21; s7 += carry6; s6 -= carry6 << 21
    carry8 = (s8 + (1 << 20)) >> 21; s9 += carry8; s8 -= carry8 << 21
    carry10 = (s10 + (1 << 20)) >> 21; s11 += carry10; s10 -= carry10 << 21

    carry1 = (s1 + (1 << 20)) >> 21; s2 += carry1; s1 -= carry1 << 21
    carry3 = (s3 + (1 << 20)) >> 21; s4 += carry3; s3 -= carry3 << 21
    carry5 = (s5 + (1 << 20)) >> 21; s6 += carry5; s5 -= carry5 << 21
    carry7 = (s7 + (1 << 20)) >> 21; s8 += carry7; s7 -= carry7 << 21
    carry9 = (s9 + (1 << 20)) >> 21; s10 += carry9; s9 -= carry9 << 21
    carry11 = (s11 + (1 << 20)) >> 21; s12 += carry11; s11 -= carry11 << 21

    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    s12 = 0

    carry0 = s0 >> 21; s1 += carry0; s0 -= carry0 << 21
    carry1 = s1 >> 21; s2 += carry1; s1 -= carry1 << 21
    carry2 = s2 >> 21; s3 += carry2; s2 -= carry2 << 21
    carry3 = s3 >> 21; s4 += carry3; s3 -= carry3 << 21
    carry4 = s4 >> 21; s5 += carry4; s4 -= carry4 << 21
    carry5 = s5 >> 21; s6 += carry5; s5 -= carry5 << 21
    carry6 = s6 >> 21; s7 += carry6; s6 -= carry6 << 21
    carry7 = s7 >> 21; s8 += carry7; s7 -= carry7 << 21
    carry8 = s8 >> 21; s9 += carry8; s8 -= carry8 << 21
    carry9 = s9 >> 21; s10 += carry9; s9 -= carry9 << 21
    carry10 = s10 >> 21; s11 += carry10; s10 -= carry10 << 21
    carry11 = s11 >> 21; s12 += carry11; s11 -= carry11 << 21

    s0 += s12 * 666643
    s1 += s12 * 470296
    s2 += s12 * 654183
    s3 -= s12 * 997805
    s4 += s12 * 136657
    s5 -= s12 * 683901
    // s12 = 0;

    carry0 = s0 >> 21; s1 += carry0; s0 -= carry0 << 21
    carry1 = s1 >> 21; s2 += carry1; s1 -= carry1 << 21
    carry2 = s2 >> 21; s3 += carry2; s2 -= carry2 << 21
    carry3 = s3 >> 21; s4 += carry3; s3 -= carry3 << 21
    carry4 = s4 >> 21; s5 += carry4; s4 -= carry4 << 21
    carry5 = s5 >> 21; s6 += carry5; s5 -= carry5 << 21
    carry6 = s6 >> 21; s7 += carry6; s6 -= carry6 << 21
    carry7 = s7 >> 21; s8 += carry7; s7 -= carry7 << 21
    carry8 = s8 >> 21; s9 += carry8; s8 -= carry8 << 21
    carry9 = s9 >> 21; s10 += carry9; s9 -= carry9 << 21
    carry10 = s10 >> 21; s11 += carry10; s10 -= carry10 << 21

    s(0) = s0.toByte
    s(1) = (s0 >> 8).toByte
    s(2) = ((s0 >> 16) | (s1 << 5)).toByte
    s(3) = (s1 >> 3).toByte
    s(4) = (s1 >> 11).toByte
    s(5) = ((s1 >> 19) | (s2 << 2)).toByte
    s(6) = (s2 >> 6).toByte
    s(7) = ((s2 >> 14) | (s3 << 7)).toByte
    s(8) = (s3 >> 1).toByte
    s(9) = (s3 >> 9).toByte
    s(10) = ((s3 >> 17) | (s4 << 4)).toByte
    s(11) = (s4 >> 4).toByte
    s(12) = (s4 >> 12).toByte
    s(13) = ((s4 >> 20) | (s5 << 1)).toByte
    s(14) = (s5 >> 7).toByte
    s(15) = ((s5 >> 15) | (s6 << 6)).toByte
    s(16) = (s6 >> 2).toByte
    s(17) = (s6 >> 10).toByte
    s(18) = ((s6 >> 18) | (s7 << 3)).toByte
    s(19) = (s7 >> 5).toByte
    s(20) = (s7 >> 13).toByte
    s(21) = s8.toByte
    s(22) = (s8 >> 8).toByte
    s(23) = ((s8 >> 16) | (s9 << 5)).toByte
    s(24) = (s9 >> 3).toByte
    s(25) = (s9 >> 11).toByte
    s(26) = ((s9 >> 19) | (s10 << 2)).toByte
    s(27) = (s10 >> 6).toByte
    s(28) = ((s10 >> 14) | (s11 << 7)).toByte
    s(29) = (s11 >> 1).toByte
    s(30) = (s11 >> 9).toByte
    s(31) = (s11 >> 17).toByte
  }

  private[vendored] def getHashedScalar(privateKey: Array[Byte]) = {
    val digest = new Sha512()
    digest.update(privateKey, 0, FIELD_LEN)
    val h: Array[Byte] = digest.digest()
    // https://tools.ietf.org/html/rfc8032#section-5.1.2.
    // Clear the lowest three bits of the first octet.
    h(0) = (h(0) & 248).toByte
    // Clear the highest bit of the last octet.
    h(31) = (h(31) & 127).toByte
    // Set the second highest bit if the last octet.
    h(31) = (h(31) | 64).toByte
    h
  }

  /**
   * Returns the EdDSA signature for the {@code message} based on the {@code hashedPrivateKey}.
   *
   * @param message          to sign
   * @param publicKey        [[Ed25519.scalarMultToBytes]] of {@code hashedPrivateKey}
   * @param hashedPrivateKey [[Ed25519.getHashedScalar]] of the private key
   * @return signature for the {@code message}.
   */
  private[vendored] def sign(message: Array[Byte], publicKey: Array[Byte], hashedPrivateKey: Array[Byte]) = {
    // Copying the message to make it thread-safe. Otherwise, if the caller modifies the message
    // between the first and the second hash then it might leak the private key.
    val messageCopy = util.Arrays.copyOfRange(message, 0, message.length)
    val digest0 = new Sha512()
    digest0.update(hashedPrivateKey, FIELD_LEN, FIELD_LEN)
    digest0.update(messageCopy)
    val r = digest0.digest()
    reduce(r)

    val rB = util.Arrays.copyOfRange(scalarMultWithBase(r).toBytes, 0, FIELD_LEN)
    val digest = new Sha512()
    digest.update(rB)
    digest.update(publicKey)
    digest.update(messageCopy)
    val hram = digest.digest()
    reduce(hram)
    val s = new Array[Byte](FIELD_LEN)
    mulAdd(s, hram, hashedPrivateKey, r)
    rB ++ s
  }


  // The order of the generator as unsigned bytes in little endian order.
  // (2^252 + 0x14def9dea2f79cd65812631a5cf5d3ed, cf. RFC 7748)
  private[vendored] val GROUP_ORDER = Array[Byte](
    0xed.toByte, 0xd3.toByte, 0xf5.toByte, 0x5c.toByte,
    0x1a.toByte, 0x63.toByte, 0x12.toByte, 0x58.toByte,
    0xd6.toByte, 0x9c.toByte, 0xf7.toByte, 0xa2.toByte,
    0xde.toByte, 0xf9.toByte, 0xde.toByte, 0x14.toByte,
    0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte,
    0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte,
    0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte,
    0x00.toByte, 0x00.toByte, 0x00.toByte, 0x10.toByte)

  // Checks whether s represents an integer smaller than the order of the group.
  // This is needed to ensure that EdDSA signatures are non-malleable, as failing to check
  // the range of S allows to modify signatures (cf. RFC 8032, Section 5.2.7 and Section 8.4.)
  // @param s an integer in little-endian order.
  private def isSmallerThanGroupOrder(s: Array[Byte]): Boolean = {
    var j = FIELD_LEN - 1
    while j >= 0 do {
      // compare unsigned bytes
      val a = s(j) & 0xff
      val b = GROUP_ORDER(j) & 0xff
      if a != b then {
        return a < b
      }
      j -= 1
    }
    false
  }

  /**
   * Returns true if the EdDSA {@code signature} with {@code message}, can be verified with
   * {@code publicKey}.
   *
   */
  private def verifyRaw(message: Array[Byte], signature: Array[Byte], publicKey: Array[Byte]): Boolean = {
    if signature.length != SIGNATURE_LEN then {
      return false
    }
    val s: Array[Byte] = util.Arrays.copyOfRange(signature, FIELD_LEN, SIGNATURE_LEN)
    if !isSmallerThanGroupOrder(s) then {
      return false
    }
    val digest = new Sha512()
    digest.update(signature, 0, FIELD_LEN)
    digest.update(publicKey)
    digest.update(message)
    val h = digest.digest()
    reduce(h)

    val negPublicKey = XYZT.fromBytesNegateVarTime(publicKey)
    val xyz = doubleScalarMultVarTime(h, negPublicKey, s)
    val expectedR = xyz.toBytes
    {
      var i = 0
      while i < FIELD_LEN do {
        if expectedR(i) != signature(i) then return false
        i += 1
      }
    }
    true
  }

  // ---- public API (added) ----

  val SeedLength: Int      = SECRET_KEY_LEN
  val PublicKeyLength: Int = PUBLIC_KEY_LEN
  val SignatureLength: Int = SIGNATURE_LEN

  /** RFC 8032 public key for the 32 byte private key (seed). */
  def publicKey(seed: Array[Byte]): Array[Byte] = {
    require(seed.length == SeedLength, s"Ed25519 private keys are $SeedLength bytes long, got ${seed.length}")
    scalarMultWithBaseToBytes(getHashedScalar(seed))
  }

  /** RFC 8032 Ed25519 signature of `message` with the 32 byte private key (seed). */
  def sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] = {
    require(seed.length == SeedLength, s"Ed25519 private keys are $SeedLength bytes long, got ${seed.length}")
    val hashed = getHashedScalar(seed)
    sign(message, scalarMultWithBaseToBytes(hashed), hashed)
  }

  /** Verifies an RFC 8032 Ed25519 signature; returns false for malformed keys or signatures. */
  def verify(publicKey: Array[Byte], message: Array[Byte], signature: Array[Byte]): Boolean =
    publicKey.length == PublicKeyLength && {
      try verifyRaw(message, signature, publicKey)
      catch case _: IllegalArgumentException => false
    }
}
