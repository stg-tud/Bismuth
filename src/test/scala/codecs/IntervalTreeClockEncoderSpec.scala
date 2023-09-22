package de.tu_darmstadt.stg.daimpl
package codecs

import causality.IntervalTreeClock
import causality.IntervalTreeClockGenerators.genIntervalTreeClock
import codecs.{EncoderSpec, IntervalTreeClockEncoder}

class IntervalTreeClockEncoderSpec extends EncoderSpec[IntervalTreeClock](using IntervalTreeClockEncoder, genIntervalTreeClock) {}
