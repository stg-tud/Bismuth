package de.tu_darmstadt.stg.daimpl
package causality.benchmarks

import causality.IntervalTreeClock
import causality.dots.Defs.Id
import causality.dots.VectorClock

import org.openjdk.jmh.annotations.*

import scala.collection.mutable
import scala.util.Random

@State(Scope.Thread)
class ForkEventJoinProgram {
  // Initial replica count, Event, Fork, Join, Atomic Send & Receive
  @Param(Array("1,25,25,25,25", "1,70,5,5,20", "10,60,0,0,40", "100,60,0,0,40"))
  var generationParams: String = ""
  private lazy val probabilities: Array[Int] = {
    generationParams.split(',').drop(1).map(_.toInt)
  }
  lazy val initialReplicaCount: Int = {
    generationParams.split(',')(0).toInt
  }

  // Initialized per run by initializeReplicaArrays
  var vcReplicas: Array[(Id, VectorClock)]  = _
  var itcReplicas: Array[IntervalTreeClock] = _

  private var _initialVcStamps: Array[(Id, VectorClock)]  = _
  private var _initialItcStamps: Array[IntervalTreeClock] = _

  @Param(Array("10000", "20000", "40000", "80000"))
  var programLength: Int = 0

  var commands: Array[ForkEventJoinCommand] = Array.empty
  private var maxNumReplicas                = -1

  private val random = Random(42) // We want this to be deterministic

  @Setup(Level.Trial)
  def buildProgram(): Unit = {
    val program = Array.ofDim[ForkEventJoinCommand](programLength)
    var progIdx = 0

    val hundredPercent     = probabilities.sum
    val decisionBoundaries = probabilities.scanLeft(0)((a, b) => a + b)

    var numReplicas = initialReplicaCount
    maxNumReplicas = initialReplicaCount

    while (progIdx < programLength) {
      val decision = random.nextInt(hundredPercent)
      // Replica to perform command on
      val performingReplica = random.nextInt(numReplicas)
      if (decision < decisionBoundaries(1)) { // Event
        program(progIdx) = EventCmd(performingReplica)
        progIdx += 1
      } else if (decision < decisionBoundaries(2)) { // Fork
        program(progIdx) = ForkCmd(performingReplica)
        numReplicas += 1
        maxNumReplicas = Math.max(maxNumReplicas, numReplicas)
        progIdx += 1
      } else if (decision < decisionBoundaries(3)) { // Join
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
      } else { // Send and receive
        if (numReplicas >= 2) {
          var receiver = performingReplica
          while (receiver == performingReplica) {
            receiver = random.nextInt(numReplicas)
          }
          program(progIdx) = SendAndReceiveCmd(performingReplica, receiver)
          progIdx += 1
        }
      }

      require(numReplicas >= 1)
    }

    commands = program

    // Set initial VectorClock stamps
    _initialVcStamps = new Array[(Id, VectorClock)](initialReplicaCount)
    for (i <- 0 until initialReplicaCount) {
      _initialVcStamps(i) = (random.nextInt(), VectorClock())
    }

    // Set initial IntervalTreeClock stamps
    val forkQueue = new mutable.Queue[IntervalTreeClock](initialReplicaCount)
    forkQueue.enqueue(IntervalTreeClock.Clock.seed)
    while (forkQueue.size < initialReplicaCount) {
      val stamp  = forkQueue.dequeue()
      val (a, b) = stamp.fork
      forkQueue.enqueue(a, b)
    }
    _initialItcStamps = forkQueue.toArray
  }

  @Setup(Level.Invocation)
  def initializeReplicaArrays(): Unit = {
    itcReplicas = new Array[IntervalTreeClock](maxNumReplicas)
    System.arraycopy(_initialItcStamps, 0, itcReplicas, 0, initialReplicaCount)

    vcReplicas = new Array[(Id, VectorClock)](maxNumReplicas)
    System.arraycopy(_initialVcStamps, 0, vcReplicas, 0, initialReplicaCount)
  }
}

sealed trait ForkEventJoinCommand
case class EventCmd(replica: Int)                          extends ForkEventJoinCommand
case class ForkCmd(replica: Int)                           extends ForkEventJoinCommand
case class JoinCmd(replica1: Int, replica2: Int)           extends ForkEventJoinCommand
case class SendAndReceiveCmd(replica1: Int, replica2: Int) extends ForkEventJoinCommand
