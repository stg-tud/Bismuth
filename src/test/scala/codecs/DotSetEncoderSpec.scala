package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Defs.Id
import causality.dots.DotSet
import causality.dots.Generators.given
import causality.dots.impl.ArrayRanges

import org.scalacheck.{Arbitrary, Gen}

class DotSetEncoderSpec extends VariableSizeEncoderSpec[DotSet] {}
