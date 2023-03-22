package de.tu_darmstadt.stg.daimpl
package causality.benchmarks

import causality.ForkEventJoinClock
import codecs.{Encoder, JavaSerializationEncoder}

class ProgramRunnerWithObjectSerialization[T: ForkEventJoinClock](
    program: ForkEventJoinProgram,
    replicas: Array[T],
    encoder: JavaSerializationEncoder[T]
) extends ProgramRunnerWithSerialization[T](program, replicas, encoder) {}
