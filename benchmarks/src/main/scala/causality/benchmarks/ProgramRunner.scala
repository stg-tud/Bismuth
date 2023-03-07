package de.tu_darmstadt.stg.daimpl
package causality.benchmarks

import causality.ForkEventJoinClock

class ProgramRunner[T: ForkEventJoinClock](program: ForkEventJoinProgram, val replicas: Array[T]) {
  var numReplicas = 1

  def runProgram(): Unit = {
    program.commands.foreach(command => exec(command))
  }

  private inline def exec(cmd: ForkEventJoinCommand): Unit = cmd match {
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
  }
}
