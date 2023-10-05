package com.github.ckuessner
package causality.benchmarks

import causality.ForkEventJoinClock
import codecs.Encoder

class ProgramRunnerWithSerialization[T: ForkEventJoinClock](
    program: ForkEventJoinProgram,
    val replicas: Array[T],
    encoder: Encoder[T]) {
  var numReplicas: Int   = program.initialReplicaCount
  var commandIndex: Int  = 0
  var bandwidthUsed: Long = 0

  def step(): Unit = {
    exec(program.commands(commandIndex))
    commandIndex += 1
  }

  def runProgram(): Unit = {
    program.commands.foreach(command => exec(command))
  }

  private inline def exec(cmd: ForkEventJoinCommand): Unit = cmd match {
    case EventCmd(replica) =>
      replicas(replica) = replicas(replica).event
    case ForkCmd(replica) =>
      val (existingReplica, newReplica) = replicas(replica).fork
      replicas(replica) = existingReplica
      replicas(numReplicas) = serializeAndDeserialize(newReplica)
      numReplicas += 1
    case JoinCmd(replica1, replica2) =>
      // Serialize replica2, since we assume replica2 merges into replica1 and therefore r2 sends stamp to r1
      val decodedReplica2 = serializeAndDeserialize(replicas(replica2))
      replicas(replica1) = replicas(replica1).join(decodedReplica2)
      // Delete replica entry by swapping with last replica and decrementing numReplicas
      replicas(replica2) = replicas(numReplicas - 1)
      numReplicas -= 1
    case SendAndReceiveCmd(replica1, replica2) =>
      val (localStamp, stampToSend) = replicas(replica1).send
      replicas(replica1) = localStamp
      val receivedStamp = serializeAndDeserialize(stampToSend)
      replicas(replica2) = replicas(replica2).receive(receivedStamp)
  }

  private inline def serializeAndDeserialize(toSerialize: T): T = {
    val encoded = encoder.writeArray(toSerialize)
    bandwidthUsed += encoded.length
    encoder.readArray(encoded)
  }

}
