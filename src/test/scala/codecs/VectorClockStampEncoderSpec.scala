package de.tu_darmstadt.stg.daimpl
package codecs

import causality.dots.Generators.given
import causality.dots.VectorClock

import de.tu_darmstadt.stg.daimpl.causality.dots.Defs.Id

class VectorClockStampEncoderSpec extends VariableSizeEncoderSpec[(Id, VectorClock)] {}
