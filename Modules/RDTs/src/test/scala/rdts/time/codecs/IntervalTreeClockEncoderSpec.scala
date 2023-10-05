package com.github.ckuessner
package codecs

import causality.IntervalTreeClock
import causality.IntervalTreeClockGenerators.genIntervalTreeClock
import codecs.IntervalTreeClockEncoder

class IntervalTreeClockEncoderSpec extends EncoderSpec[IntervalTreeClock](using IntervalTreeClockEncoder, genIntervalTreeClock) {}
