package de.tu_darmstadt.stg.daimpl
package causality.benchmarks

import causality.IntervalTreeClock.given
import causality.dots.Defs.Id
import causality.dots.VectorClock
import causality.dots.VectorClock.{*, given}
import causality.{EventTree, ForkEventJoinClock, IdTree, IntervalTreeClock}
import codecs.{FastIntervalTreeClockEncoder, VectorClockStampEncoder, JavaSerializationEncoder}

import org.openjdk.jmh.annotations.*

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.runtime.Arrays
import scala.util.Random

@Warmup(time = 1, iterations = 3)
@Measurement(time = 1, iterations = 3)
@Fork(3)
@BenchmarkMode(Array(Mode.AverageTime))
class ForkEventJoinSendReceiveBenchmark {
  @Benchmark
  def testIntervalTreeClock(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunner(program, program.itcReplicas)
    runner.runProgram()
  }

  @Benchmark
  def testVectorClock(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunner(program, program.vcReplicas)
    runner.runProgram()
  }

  @Benchmark
  def testVectorClockWithSerialization(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunnerWithSerialization(program, program.vcReplicas, VectorClockStampEncoder)
    runner.runProgram()
  }

  @Benchmark
  def testIntervalTreeClockWithSerialization(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunnerWithSerialization(program, program.itcReplicas, FastIntervalTreeClockEncoder)
    runner.runProgram()
  }

  @Benchmark
  def testVectorClockWithObjectSerialization(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunnerWithObjectSerialization(program, program.vcReplicas, JavaSerializationEncoder[(Id, VectorClock)])
    runner.runProgram()
  }

  @Benchmark
  def testIntervalTreeClockWithObjectSerialization(program: ForkEventJoinProgram): Unit = {
    val runner = new ProgramRunnerWithObjectSerialization(program, program.itcReplicas, JavaSerializationEncoder[IntervalTreeClock])
    runner.runProgram()
  }
}
