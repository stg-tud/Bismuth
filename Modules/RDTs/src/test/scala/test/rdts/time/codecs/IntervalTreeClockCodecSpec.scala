package test.rdts.time.codecs

import rdts.time.IntervalTreeClock
import rdts.time.codecs.IntervalTreeClockCodec
import test.rdts.time.IntervalTreeClockGenerators.genIntervalTreeClock

class IntervalTreeClockCodecSpec
    extends CodecSpec[IntervalTreeClock](using IntervalTreeClockCodec, genIntervalTreeClock) {}
