package de.tu_darmstadt.stg.daimpl
package causality.microbenchmarks

import causality.IntervalTreeClock.given
import causality.dots.Defs.Id
import causality.dots.VectorClock
import causality.dots.VectorClock.{*, given}
import causality.{EventTree, ForkEventJoinCausality, IdTree, IntervalTreeClock}

import org.openjdk.jmh.annotations.*

import scala.reflect.ClassTag
import scala.util.Random

@Warmup(time = 1)
@Measurement(time = 1, iterations = 3)
@Fork(1)
@BenchmarkMode(Array(Mode.AverageTime))
class ForkEventJoinSendReceiveBenchmark {
  @Benchmark
  def testIntervalTreeClock(program: ForkEventJoinProgram): Unit = {
    runProgram(program, IntervalTreeClock.itcFejc.seed)
  }

  @Benchmark
  def testVectorClock(program: ForkEventJoinProgram): Unit = {
    runProgram(program, vcFejc.seed)
  }

  private inline def runProgram[T: ForkEventJoinCausality: ClassTag](
      program: ForkEventJoinProgram,
      seedStamp: T
  ): Unit = {
    // TODO: Add encoding and decoding to send receive once implemented for ITC
    val replicas = Array.ofDim[T](program.maxNumReplicas)
    replicas(0) = seedStamp
    var numReplicas = 1

    program.commands.foreach {
      case EventCmd(replica) =>
        replicas(replica) = replicas(replica).event
      case ForkCmd(replica) =>
        val (existingReplica, newReplica) = replicas(replica).fork
        replicas(replica) = existingReplica
        replicas(numReplicas) = newReplica
        numReplicas += 1
      case JoinCmd(replica1, replica2) =>
        replicas(replica1) = replicas(replica1).join(replicas(replica2))
        // Delete replica entry by swapping with last replica and decrementing numReplicas
        replicas(replica2) = replicas(numReplicas - 1)
        numReplicas -= 1
      case SendAndReceiveCmd(replica1, replica2) =>
        val (localStamp, stampToSend) = replicas(replica1).send
        replicas(replica1) = localStamp
        replicas(replica2) = replicas(replica2).receive(stampToSend)
      case _ =>
        ???
    }
  }
}

@State(Scope.Thread)
class ForkEventJoinProgram {
  // Event, Fork, Join, Send followed by Receive
  @Param(Array("25,25,25,25"))
  var _probabilities: String = ""
  lazy val probabilities: Array[Int] = {
    _probabilities.split(',').map(_.toInt)
  }

  @Param(Array("1000", "10000", "100000"))
  var programLength: Int = 0

  var commands: Array[ForkEventJoinCommand] = Array.empty
  var maxNumReplicas                        = 0

  private val random = Random(42) // We want this to be deterministic

  @Setup
  def buildProgram(): Unit = {
    val program = Array.ofDim[ForkEventJoinCommand](programLength)
    var progIdx = 0

    val hundredPercent     = probabilities.sum
    val decisionBoundaries = probabilities.scanLeft(0)((a, b) => a + b)

    var numReplicas = 1

    while (progIdx < programLength) {
      val decision = random.nextInt(hundredPercent)
      // Replica to perform command on
      val performingReplica = random.nextInt(numReplicas)
      if (decision < decisionBoundaries(0)) { // Event
        program(progIdx) = EventCmd(performingReplica)
        progIdx += 1
      } else if (decision < decisionBoundaries(1)) { // Fork
        program(progIdx) = ForkCmd(performingReplica)
        numReplicas += 1
        maxNumReplicas = Math.max(maxNumReplicas, numReplicas)
        progIdx += 1
      } else if (decision < decisionBoundaries(2)) { // Join
        // Check if we actually have 2 or more replicas to join
        if (numReplicas >= 2) {
          var otherReplica = performingReplica
          while (otherReplica == performingReplica) {
            otherReplica = random.nextInt(numReplicas)
          }
          program(progIdx) = JoinCmd(performingReplica, otherReplica)
          numReplicas -= 1
          progIdx += 1
        }
      } else if (decision < decisionBoundaries(3)) { // Send and receive
        if (numReplicas >= 2) {
          var receiver = performingReplica
          while (receiver == performingReplica) {
            receiver = random.nextInt(numReplicas)
          }
          program(progIdx) = SendAndReceiveCmd(performingReplica, receiver)
          progIdx += 1
        }
      }
    }

    commands = program
  }
}

trait ForkEventJoinCommand
case class EventCmd(replica: Int)                          extends ForkEventJoinCommand
case class ForkCmd(replica: Int)                           extends ForkEventJoinCommand
case class JoinCmd(replica1: Int, replica2: Int)           extends ForkEventJoinCommand
case class SendAndReceiveCmd(replica1: Int, replica2: Int) extends ForkEventJoinCommand
