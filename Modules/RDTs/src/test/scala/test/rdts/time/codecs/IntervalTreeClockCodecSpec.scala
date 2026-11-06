package test.rdts.time.codecs

import rdts.time.IntervalTreeClock
import test.rdts.time.IntervalTreeClockGenerators.genIntervalTreeClock
import rdts.time.codecs.IntervalTreeClockCodec

class IntervalTreeClockCodecSpec
    extends CodecSpec[IntervalTreeClock](using IntervalTreeClockCodec, genIntervalTreeClock) {}
