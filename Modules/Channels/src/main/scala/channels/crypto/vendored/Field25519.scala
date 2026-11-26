// Derived from Tink (https://github.com/google/tink, java_src), via the Scala port
// https://github.com/ckuessner/unofficial-tink-scala (commit ed64d86a25f0e38688dbcb23214974196a502706).
// Copyright 2017 Google Inc., licensed under the Apache License, Version 2.0
// (http://www.apache.org/licenses/LICENSE-2.0). Modified: moved package, added copyConditional from Tink's Curve25519.
package channels.crypto.vendored


import java.util

/**
 * Defines field 25519 function based on <a
 * href="https://github.com/agl/curve25519-donna/blob/master/curve25519-donna.c">curve25519-donna C
 * implementation</a> (mostly identical).
 *
 * <p>Field elements are written as an array of signed, 64-bit limbs (an array of longs), least
 * significant first. The value of the field element is:
 *
 * <pre>
 * x[0] + 2^26·x[1] + 2^51·x[2] + 2^77·x[3] + 2^102·x[4] + 2^128·x[5] + 2^153·x[6] + 2^179·x[7] +
 * 2^204·x[8] + 2^230·x[9],
 * </pre>
 *
 * <p>i.e. the limbs are 26, 25, 26, 25, ... bits wide.
 */
object Field25519 {
  /**
   * During Field25519 computation, the mixed radix representation may be in different forms:
   * <ul>
   * <li> Reduced-size form: the array has size at most 10.
   * <li> Non-reduced-size form: the array is not reduced modulo 2^255 - 19 and has size at most
   * 19.
   * </ul>
   *
   * TODO(quannguyen):
   * <ul>
   * <li> Clarify ill-defined terminologies.
   * <li> The reduction procedure is different from DJB's paper
   * (http://cr.yp.to/ecdh/curve25519-20060209.pdf). The coefficients after reducing degree and
   * reducing coefficients aren't guaranteed to be in range {-2^25, ..., 2^25}. We should check to
   * see what's going on.
   * <li> Consider using method mult() everywhere and making product() private.
   * </ul>
   */
  private[vendored] val FIELD_LEN: Int = 32
  private[vendored] val LIMB_CNT: Int = 10
  private val TWO_TO_25: Long = 1 << 25
  private val TWO_TO_26: Long = TWO_TO_25 << 1

  private val EXPAND_START: Array[Int] = Array(0, 3, 6, 9, 12, 16, 19, 22, 25, 28)
  private val EXPAND_SHIFT: Array[Int] = Array(0, 2, 3, 5, 6, 0, 1, 3, 4, 6)
  private val MASK: Array[Int] = Array(0x3ffffff, 0x1ffffff)
  private val SHIFT: Array[Int] = Array(26, 25)

  /**
   * Sums two numbers: output = in1 + in2
   *
   * On entry: in1, in2 are in reduced-size form.
   */
  private[vendored] def sum(output: Array[Long], in1: Array[Long], in2: Array[Long]): Unit = {
    for i <- 0 until LIMB_CNT do {
      output(i) = in1(i) + in2(i)
    }
  }

  /**
   * Sums two numbers: output += in
   *
   * On entry: in is in reduced-size form.
   */
  private[vendored] def sum(output: Array[Long], in: Array[Long]): Unit = {
    sum(output, output, in)
  }

  /**
   * Find the difference of two numbers: output = in1 - in2
   * (note the order of the arguments!).
   *
   * On entry: in1, in2 are in reduced-size form.
   */
  private[vendored] def sub(output: Array[Long], in1: Array[Long], in2: Array[Long]): Unit = {
    for i <- 0 until LIMB_CNT do {
      output(i) = in1(i) - in2(i)
    }
  }

  /**
   * Find the difference of two numbers: output = in - output
   * (note the order of the arguments!).
   *
   * On entry: in, output are in reduced-size form.
   */
  private[vendored] def sub(output: Array[Long], in: Array[Long]): Unit = {
    sub(output, in, output)
  }

  /**
   * Multiply a number by a scalar: output = in * scalar
   */
  private[vendored] def scalarProduct(output: Array[Long], in: Array[Long], scalar: Long): Unit = {
    for i <- 0 until LIMB_CNT do {
      output(i) = in(i) * scalar
    }
  }

  /**
   * Multiply two numbers: out = in2 * in
   *
   * output must be distinct to both inputs. The inputs are reduced coefficient form,
   * the output is not.
   *
   * out[x] <= 14 * the largest product of the input limbs.
   */
  private[vendored] def product(out: Array[Long], in2: Array[Long], in: Array[Long]): Unit = {
    out(0) = in2(0) * in(0);
    out(1) = in2(0) * in(1)
      + in2(1) * in(0);
    out(2) = 2 * in2(1) * in(1)
      + in2(0) * in(2)
      + in2(2) * in(0);
    out(3) = in2(1) * in(2)
      + in2(2) * in(1)
      + in2(0) * in(3)
      + in2(3) * in(0);
    out(4) = in2(2) * in(2)
      + 2 * (in2(1) * in(3) + in2(3) * in(1))
      + in2(0) * in(4)
      + in2(4) * in(0);
    out(5) = in2(2) * in(3)
      + in2(3) * in(2)
      + in2(1) * in(4)
      + in2(4) * in(1)
      + in2(0) * in(5)
      + in2(5) * in(0);
    out(6) = 2 * (in2(3) * in(3) + in2(1) * in(5) + in2(5) * in(1))
      + in2(2) * in(4)
      + in2(4) * in(2)
      + in2(0) * in(6)
      + in2(6) * in(0);
    out(7) = in2(3) * in(4)
      + in2(4) * in(3)
      + in2(2) * in(5)
      + in2(5) * in(2)
      + in2(1) * in(6)
      + in2(6) * in(1)
      + in2(0) * in(7)
      + in2(7) * in(0);
    out(8) = in2(4) * in(4)
      + 2 * (in2(3) * in(5) + in2(5) * in(3) + in2(1) * in(7) + in2(7) * in(1))
      + in2(2) * in(6)
      + in2(6) * in(2)
      + in2(0) * in(8)
      + in2(8) * in(0);
    out(9) = in2(4) * in(5)
      + in2(5) * in(4)
      + in2(3) * in(6)
      + in2(6) * in(3)
      + in2(2) * in(7)
      + in2(7) * in(2)
      + in2(1) * in(8)
      + in2(8) * in(1)
      + in2(0) * in(9)
      + in2(9) * in(0);
    out(10) =
      2 * (in2(5) * in(5) + in2(3) * in(7) + in2(7) * in(3) + in2(1) * in(9) + in2(9) * in(1))
        + in2(4) * in(6)
        + in2(6) * in(4)
        + in2(2) * in(8)
        + in2(8) * in(2);
    out(11) = in2(5) * in(6)
      + in2(6) * in(5)
      + in2(4) * in(7)
      + in2(7) * in(4)
      + in2(3) * in(8)
      + in2(8) * in(3)
      + in2(2) * in(9)
      + in2(9) * in(2);
    out(12) = in2(6) * in(6)
      + 2 * (in2(5) * in(7) + in2(7) * in(5) + in2(3) * in(9) + in2(9) * in(3))
      + in2(4) * in(8)
      + in2(8) * in(4);
    out(13) = in2(6) * in(7)
      + in2(7) * in(6)
      + in2(5) * in(8)
      + in2(8) * in(5)
      + in2(4) * in(9)
      + in2(9) * in(4);
    out(14) = 2 * (in2(7) * in(7) + in2(5) * in(9) + in2(9) * in(5))
      + in2(6) * in(8)
      + in2(8) * in(6);
    out(15) = in2(7) * in(8)
      + in2(8) * in(7)
      + in2(6) * in(9)
      + in2(9) * in(6);
    out(16) = in2(8) * in(8)
      + 2 * (in2(7) * in(9) + in2(9) * in(7));
    out(17) = in2(8) * in(9)
      + in2(9) * in(8);
    out(18) = 2 * in2(9) * in(9);
  }

  /**
   * Reduce a field element by calling reduceSizeByModularReduction and reduceCoefficients.
   *
   * @param input  An input array of any length. If the array has 19 elements, it will be used as
   *               temporary buffer and its contents changed.
   * @param output An output array of size LIMB_CNT. After the call |output[i]| < 2^26 will hold.
   *
   */
  private[vendored] def reduce(input: Array[Long], output: Array[Long]): Unit = {
    val tmp: Array[Long] =
      if input.length == 19 then input
      else {
        val copy = new Array[Long](19)
        System.arraycopy(input, 0, copy, 0, input.length)
        copy
      }
    reduceSizeByModularReduction(tmp)
    reduceCoefficients(tmp)
    System.arraycopy(tmp, 0, output, 0, LIMB_CNT)
  }

  /**
   * Reduce a long form to a reduced-size form by taking the input mod 2^255 - 19.
   *
   * On entry: |output[i]| < 14*2^54
   * On exit: |output[0..8]| < 280*2^54
   */
  private[vendored] def reduceSizeByModularReduction(output: Array[Long]): Unit = {
    // The coefficients x[10], x[11],..., x[18] are eliminated by reduction modulo 2^255 - 19.
    // For example, the coefficient x[18] is multiplied by 19 and added to the coefficient x[8].
    //
    // Each of these shifts and adds ends up multiplying the value by 19.
    //
    // For output[0..8], the absolute entry value is < 14*2^54 and we add, at most, 19*14*2^54 thus,
    // on exit, |output[0..8]| < 280*2^54.
    output(8) += output(18) << 4
    output(8) += output(18) << 1
    output(8) += output(18)
    output(7) += output(17) << 4
    output(7) += output(17) << 1
    output(7) += output(17)
    output(6) += output(16) << 4
    output(6) += output(16) << 1
    output(6) += output(16)
    output(5) += output(15) << 4
    output(5) += output(15) << 1
    output(5) += output(15)
    output(4) += output(14) << 4
    output(4) += output(14) << 1
    output(4) += output(14)
    output(3) += output(13) << 4
    output(3) += output(13) << 1
    output(3) += output(13)
    output(2) += output(12) << 4
    output(2) += output(12) << 1
    output(2) += output(12)
    output(1) += output(11) << 4
    output(1) += output(11) << 1
    output(1) += output(11)
    output(0) += output(10) << 4
    output(0) += output(10) << 1
    output(0) += output(10)
  }

  /**
   * Reduce all coefficients of the short form input so that |x| < 2^26.
   *
   * On entry: |output[i]| < 280*2^54
   */
  private[vendored] def reduceCoefficients(output: Array[Long]): Unit = {
    output(10) = 0
    var i = 0
    while i < LIMB_CNT do {
      var over = output(i) / TWO_TO_26
      // The entry condition (that |output[i]| < 280*2^54) means that over is, at most, 280*2^28 in
      // the first iteration of this loop. This is added to the next limb and we can approximate the
      // resulting bound of that limb by 281*2^54.
      output(i) -= over << 26
      output(i + 1) += over
      // For the first iteration, |output[i+1]| < 281*2^54, thus |over| < 281*2^29. When this is
      // added to the next limb, the resulting bound can be approximated as 281*2^54.
      //
      // For subsequent iterations of the loop, 281*2^54 remains a conservative bound and no
      // overflow occurs.
      over = output(i + 1) / TWO_TO_25
      output(i + 1) -= over << 25
      output(i + 2) += over
      i += 2
    }
    // Now |output[10]| < 281*2^29 and all other coefficients are reduced.
    output(0) += output(10) << 4
    output(0) += output(10) << 1
    output(0) += output(10)
    output(10) = 0
    // Now output[1..9] are reduced, and |output[0]| < 2^26 + 19*281*2^29 so |over| will be no more
    // than 2^16.
    val over = output(0) / TWO_TO_26
    output(0) -= over << 26
    output(1) += over
    // Now output[0,2..9] are reduced, and |output[1]| < 2^25 + 2^16 < 2^26. The bound on
    // |output[1]| is sufficient to meet our needs.
  }

  /**
   * A helpful wrapper around {@ref Field25519#product}: output = in * in2.
   *
   * On entry: |in[i]| < 2^27 and |in2[i]| < 2^27.
   *
   * The output is reduced degree (indeed, one need only provide storage for 10 limbs) and
   * |output[i]| < 2^26.
   */
  private[vendored] def mult(output: Array[Long], in: Array[Long], in2: Array[Long]): Unit = {
    val t = new Array[Long](19)
    product(t, in, in2)
    // |t[i]| < 2^26
    reduce(t, output)
  }

  /**
   * Square a number: out = in**2
   *
   * output must be distinct from the input. The inputs are reduced coefficient form, the output is
   * not.
   *
   * out[x] <= 14 * the largest product of the input limbs.
   */
  private def squareInner(out: Array[Long], in: Array[Long]): Unit = {
    out(0) = in(0) * in(0);
    out(1) = 2 * in(0) * in(1);
    out(2) = 2 * (in(1) * in(1) + in(0) * in(2));
    out(3) = 2 * (in(1) * in(2) + in(0) * in(3));
    out(4) = in(2) * in(2)
      + 4 * in(1) * in(3)
      + 2 * in(0) * in(4);
    out(5) = 2 * (in(2) * in(3) + in(1) * in(4) + in(0) * in(5));
    out(6) = 2 * (in(3) * in(3) + in(2) * in(4) + in(0) * in(6) + 2 * in(1) * in(5));
    out(7) = 2 * (in(3) * in(4) + in(2) * in(5) + in(1) * in(6) + in(0) * in(7));
    out(8) = in(4) * in(4)
      + 2 * (in(2) * in(6) + in(0) * in(8) + 2 * (in(1) * in(7) + in(3) * in(5)));
    out(9) = 2 * (in(4) * in(5) + in(3) * in(6) + in(2) * in(7) + in(1) * in(8) + in(0) * in(9));
    out(10) = 2 * (in(5) * in(5)
      + in(4) * in(6)
      + in(2) * in(8)
      + 2 * (in(3) * in(7) + in(1) * in(9)));
    out(11) = 2 * (in(5) * in(6) + in(4) * in(7) + in(3) * in(8) + in(2) * in(9));
    out(12) = in(6) * in(6)
      + 2 * (in(4) * in(8) + 2 * (in(5) * in(7) + in(3) * in(9)));
    out(13) = 2 * (in(6) * in(7) + in(5) * in(8) + in(4) * in(9));
    out(14) = 2 * (in(7) * in(7) + in(6) * in(8) + 2 * in(5) * in(9));
    out(15) = 2 * (in(7) * in(8) + in(6) * in(9));
    out(16) = in(8) * in(8) + 4 * in(7) * in(9);
    out(17) = 2 * in(8) * in(9);
    out(18) = 2 * in(9) * in(9);
  }

  /**
   * Returns in^2.
   *
   * On entry: The |in| argument is in reduced coefficients form and |in[i]| < 2^27.
   *
   * On exit: The |output| argument is in reduced coefficients form (indeed, one need only provide
   * storage for 10 limbs) and |out[i]| < 2^26.
   */
  private[vendored] def square(output: Array[Long], in: Array[Long]): Unit = {
    val t = new Array[Long](19)
    squareInner(t, in)
    // |t[i]| < 14*2^54 because the largest product of two limbs will be < 2^(27+27) and SquareInner
    // adds together, at most, 14 of those products.
    reduce(t, output)
  }

  /**
   * Takes a little-endian, 32-byte number and expands it into mixed radix form.
   */
  private[vendored] def expand(input: Array[Byte]) = {
    val output = new Array[Long](LIMB_CNT)
    for i <- 0 until LIMB_CNT do {
      output(i) = (((input(EXPAND_START(i)) & 0xff).toLong
        | (input(EXPAND_START(i) + 1) & 0xff).toLong << 8
        | (input(EXPAND_START(i) + 2) & 0xff).toLong << 16
        | (input(EXPAND_START(i) + 3) & 0xff).toLong << 24) >> EXPAND_SHIFT(i)) & MASK(i & 1)
    }
    output
  }

  /**
   * Takes a fully reduced mixed radix form number and contract it into a little-endian, 32-byte
   * array.
   *
   * On entry: |input_limbs[i]| < 2^26
   */
  @SuppressWarnings(Array("NarrowingCompoundAssignment"))
  private[vendored] def contract(inputLimbs: Array[Long]) = {
    val input = util.Arrays.copyOf(inputLimbs, LIMB_CNT)
    for j <- 0 until 2 do {
      for i <- 0 until 9 do {
        // This calculation is a time-invariant way to make input[i] non-negative by borrowing
        // from the next-larger limb.
        val carry = -((input(i) & (input(i) >> 31)) >> SHIFT(i & 1)).toInt
        input(i) = input(i) + (carry << SHIFT(i & 1))
        input(i + 1) -= carry
      }
      // There's no greater limb for input[9] to borrow from, but we can multiply by 19 and borrow
      // from input[0], which is valid mod 2^255-19.
      {
        val carry = -((input(9) & (input(9) >> 31)) >> 25).toInt
        input(9) += (carry << 25)
        input(0) -= (carry * 19)
      }

      // After the first iteration, input[1..9] are non-negative and fit within 25 or 26 bits,
      // depending on position. However, input[0] may be negative.
    }

    // The first borrow-propagation pass above ended with every limb except (possibly) input[0]
    // non-negative.
    //
    // If input[0] was negative after the first pass, then it was because of a carry from input[9].
    // On entry, input[9] < 2^26 so the carry was, at most, one, since (2**26-1) >> 25 = 1. Thus
    // input[0] >= -19.
    //
    // In the second pass, each limb is decreased by at most one. Thus the second borrow-propagation
    // pass could only have wrapped around to decrease input[0] again if the first pass left
    // input[0] negative *and* input[1] through input[9] were all zero.  In that case, input[1] is
    // now 2^25 - 1, and this last borrow-propagation step will leave input[1] non-negative.
    {
      val carry = -((input(0) & (input(0) >> 31)) >> 26).toInt
      input(0) += (carry << 26)
      input(1) -= carry
    }

    // All input[i] are now non-negative. However, there might be values between 2^25 and 2^26 in a
    // limb which is, nominally, 25 bits wide.
    for j <- 0 until 2 do {
      for i <- 0 until 9 do {
        val carry = (input(i) >> SHIFT(i & 1)).toInt
        input(i) &= MASK(i & 1)
        input(i + 1) += carry
      }
    }

    {
      val carry = (input(9) >> 25).toInt
      input(9) &= 0x1ffffff
      input(0) += 19 * carry
    }

    // If the first carry-chain pass, just above, ended up with a carry from input[9], and that
    // caused input[0] to be out-of-bounds, then input[0] was < 2^26 + 2*19, because the carry was,
    // at most, two.
    //
    // If the second pass carried from input[9] again then input[0] is < 2*19 and the input[9] ->
    // input[0] carry didn't push input[0] out of bounds.

    // It still remains the case that input might be between 2^255-19 and 2^255. In this case,
    // input[1..9] must take their maximum value and input[0] must be >= (2^255-19) & 0x3ffffff,
    // which is 0x3ffffed.
    var mask = gte(input(0).toInt, 0x3ffffed)
    for i <- 1 until LIMB_CNT do {
      mask &= eq(input(i).toInt, MASK(i & 1))
    }

    // mask is either 0xffffffff (if input >= 2^255-19) and zero otherwise. Thus this conditionally
    // subtracts 2^255-19.
    input(0) -= mask & 0x3ffffed
    input(1) -= mask & 0x1ffffff
    {
      var i = 2
      while i < LIMB_CNT do {
        input(i) -= mask & 0x3ffffff
        input(i + 1) -= mask & 0x1ffffff
        i += 2
      }
    }

    for i <- 0 until LIMB_CNT do {
      input(i) <<= EXPAND_SHIFT(i)
    }
    val output = new Array[Byte](FIELD_LEN)
    for i <- 0 until LIMB_CNT do {
      output(EXPAND_START(i)) = (output(EXPAND_START(i)) | input(i) & 0xff).toByte
      output(EXPAND_START(i) + 1) = (output(EXPAND_START(i) + 1) | (input(i) >> 8) & 0xff).toByte
      output(EXPAND_START(i) + 2) = (output(EXPAND_START(i) + 2) | (input(i) >> 16) & 0xff).toByte
      output(EXPAND_START(i) + 3) = (output(EXPAND_START(i) + 3) | (input(i) >> 24) & 0xff).toByte
    }
    output
  }

  /**
   * Computes inverse of z = z(2^255 - 21)
   *
   * Shamelessly copied from agl's code which was shamelessly copied from djb's code. Only the
   * comment format and the variable namings are different from those.
   */
  private[vendored] def inverse(out: Array[Long], z: Array[Long]): Unit = {
    val z2 = new Array[Long](Field25519.LIMB_CNT)
    val z9 = new Array[Long](Field25519.LIMB_CNT)
    val z11 = new Array[Long](Field25519.LIMB_CNT)
    val z2To5Minus1 = new Array[Long](Field25519.LIMB_CNT)
    val z2To10Minus1 = new Array[Long](Field25519.LIMB_CNT)
    val z2To20Minus1 = new Array[Long](Field25519.LIMB_CNT)
    val z2To50Minus1 = new Array[Long](Field25519.LIMB_CNT)
    val z2To100Minus1 = new Array[Long](Field25519.LIMB_CNT)
    val t0 = new Array[Long](Field25519.LIMB_CNT)
    val t1 = new Array[Long](Field25519.LIMB_CNT)

    square(z2, z) // 2
    square(t1, z2) // 4
    square(t0, t1) // 8
    mult(z9, t0, z) // 9
    mult(z11, z9, z2) // 11
    square(t0, z11) // 22
    mult(z2To5Minus1, t0, z9) // 2^5 - 2^0 = 31

    square(t0, z2To5Minus1) // 2^6 - 2^1
    square(t1, t0) // 2^7 - 2^2
    square(t0, t1) // 2^8 - 2^3
    square(t1, t0) // 2^9 - 2^4
    square(t0, t1) // 2^10 - 2^5
    mult(z2To10Minus1, t0, z2To5Minus1) // 2^10 - 2^0

    square(t0, z2To10Minus1) // 2^11 - 2^1
    square(t1, t0) // 2^12 - 2^2
    {
      var i = 2
      while i < 10 do { // 2^20 - 2^10
        square(t0, t1)
        square(t1, t0)
        i += 2
      }
    }
    mult(z2To20Minus1, t1, z2To10Minus1) // 2^20 - 2^0

    square(t0, z2To20Minus1) // 2^21 - 2^1
    square(t1, t0) // 2^22 - 2^2
    {
      var i = 2
      while i < 20 do { // 2^40 - 2^20
        square(t0, t1)
        square(t1, t0)
        i += 2
      }
    }
    mult(t0, t1, z2To20Minus1) // 2^40 - 2^0

    square(t1, t0) // 2^41 - 2^1
    square(t0, t1) // 2^42 - 2^2
    var i = 2
    while i < 10 do { // 2^50 - 2^10
      square(t1, t0)
      square(t0, t1)
      i += 2
    }
    mult(z2To50Minus1, t0, z2To10Minus1) // 2^50 - 2^0

    square(t0, z2To50Minus1) // 2^51 - 2^1
    square(t1, t0) // 2^52 - 2^2
    {
      var i = 2
      while i < 50 do { // 2^100 - 2^50
        square(t0, t1)
        square(t1, t0)
        i += 2
      }
    }
    mult(z2To100Minus1, t1, z2To50Minus1) // 2^100 - 2^0
    square(t1, z2To100Minus1) // 2^101 - 2^1
    square(t0, t1) // 2^102 - 2^2
    {
      var i = 2
      while i < 100 do { // 2^200 - 2^100
        square(t1, t0)
        square(t0, t1)
        i += 2
      }
    }
    mult(t1, t0, z2To100Minus1) // 2^200 - 2^0

    square(t0, t1) // 2^201 - 2^1
    square(t1, t0) // 2^202 - 2^2
    {
      var i = 2
      while i < 50 do { // 2^250 - 2^50
        square(t0, t1)
        square(t1, t0)
        i += 2
      }
    }
    mult(t0, t1, z2To50Minus1) // 2^250 - 2^0

    square(t1, t0) // 2^251 - 2^1
    square(t0, t1) // 2^252 - 2^2
    square(t1, t0) // 2^253 - 2^3
    square(t0, t1) // 2^254 - 2^4
    square(t1, t0) // 2^255 - 2^5
    mult(out, t1, z11) // 2^255 - 21
  }

  /**
   * Returns 0xffffffff iff a == b and zero otherwise.
   */
  private def eq(_a: Int, b: Int) = {
    var a = _a
    a = ~(a ^ b)
    a &= a << 16
    a &= a << 8
    a &= a << 4
    a &= a << 2
    a &= a << 1
    a >> 31
  }

  /**
   * returns 0xffffffff if a >= b and zero otherwise, where a and b are both non-negative.
   */
  private def gte(_a: Int, b: Int) = {
    val a = _a - b;
    // a >= 0 iff a >= b.
    ~(a >> 31)
  }

  /** Constant-time conditional copy of `b` into `a` if `icopy == 1` (from Tink's `Curve25519.copyConditional`). */
  private[vendored] def copyConditional(a: Array[Long], b: Array[Long], icopy: Int): Unit = {
    val copy = -icopy
    var i    = 0
    while i < LIMB_CNT do {
      val x = copy & (a(i).toInt ^ b(i).toInt)
      a(i) = a(i).toInt ^ x
      i += 1
    }
  }
}
