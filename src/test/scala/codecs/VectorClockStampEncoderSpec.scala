package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Defs.Id
import causality.dots.Generators.given
import causality.dots.VectorClock

class VectorClockStampEncoderSpec extends VariableSizeEncoderSpec[(Id, VectorClock)] {}
