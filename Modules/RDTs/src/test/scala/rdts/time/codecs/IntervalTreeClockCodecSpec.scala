package com.github.ckuessner
package codecs

import causality.IntervalTreeClock
import causality.IntervalTreeClockGenerators.genIntervalTreeClock
import codecs.IntervalTreeClockCodec

class IntervalTreeClockCodecSpec
    extends CodecSpec[IntervalTreeClock](using IntervalTreeClockCodec, genIntervalTreeClock) {}
