package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Dot
import causality.dots.Generators.given

import org.scalacheck.{Arbitrary, Gen}

class DotEncoderSpec extends FixedSizeEncoderSpec[Dot] {}
