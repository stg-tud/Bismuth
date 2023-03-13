package de.tu_darmstadt.stg.daimpl
package codecs

import causality.IntervalTreeClock
import causality.IntervalTreeClockGenerators.genIntervalTreeClock
import codecs.{EncoderSpec, FastIntervalTreeClockEncoder}

class FastIntervalTreeClockEncoderSpec extends EncoderSpec[IntervalTreeClock](using FastIntervalTreeClockEncoder, genIntervalTreeClock) {}
