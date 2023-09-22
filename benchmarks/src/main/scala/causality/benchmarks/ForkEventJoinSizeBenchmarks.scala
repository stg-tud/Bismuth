package de.tu_darmstadt.stg.daimpl
package causality.benchmarks

import codecs.{IntervalTreeClockEncoder, VectorClockStampEncoder}

import org.openjdk.jmh.annotations.*

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

@Fork(1)
@BenchmarkMode(Array(Mode.SingleShotTime))
class ForkEventJoinSizeBenchmarks {

  @Benchmark
  def sizeBenchmarkVectorClockWithSerialization(program: ForkEventJoinProgram): Unit = {
    val bandwidthFile = new PrintWriter(
      Files.newOutputStream(
        Paths.get(s"./results/vc-bandwidth-${program.generationParams}-${program.programLength}.csv")
      )
    )
    val numReplicasFile = new PrintWriter(
      Files.newOutputStream(
        Paths.get(s"./results/active-replicas-${program.generationParams}-${program.programLength}.csv")
      )
    )


    val runner = new ProgramRunnerWithSerialization(program, program.vcReplicas, VectorClockStampEncoder)

    bandwidthFile.println("commands,bandwidth (bytes)")
    bandwidthFile.println("0,0")

    numReplicasFile.println("commands,active replicas")
    numReplicasFile.println(s"0,${runner.numReplicas}")

    while (runner.commandIndex < program.programLength) {
      runner.step()
      bandwidthFile.println(s"${runner.commandIndex},${runner.bandwidthUsed}")
      numReplicasFile.println(s"${runner.commandIndex},${runner.numReplicas}")
    }

    bandwidthFile.close()
    numReplicasFile.close()
  }

  @Benchmark
  def countUniqueReplicas(program: ForkEventJoinProgram): Unit = {
    val numForksFile = new PrintWriter(
      Files.newOutputStream(
        Paths.get(s"./results/all-replicas-${program.generationParams}-${program.programLength}.csv")
      )
    )

    var replicaCount = program.initialReplicaCount

    numForksFile.println("commands,unique replicas")
    numForksFile.println(s"0,$replicaCount")

    program.commands.zipWithIndex.foreach { (cmd, i) =>
      cmd match {
        case ForkCmd(_) => replicaCount += 1
        case _          =>
      }
      numForksFile.println(s"${i + 1},$replicaCount")
    }
  }

  @Benchmark
  def sizeBenchmarkIntervalTreeClockWithSerialization(program: ForkEventJoinProgram): Unit = {
    val bandwidthFile = new PrintWriter(
      Files.newOutputStream(
        Paths.get(s"./results/itc-bandwidth-${program.generationParams}-${program.programLength}.csv")
      )
    )
    bandwidthFile.println("commands,bandwidth (bytes)")
    bandwidthFile.println("0,0")

    val runner = new ProgramRunnerWithSerialization(program, program.itcReplicas, IntervalTreeClockEncoder)
    while (runner.commandIndex < program.programLength) {
      runner.step()
      bandwidthFile.println(s"${runner.commandIndex},${runner.bandwidthUsed}")
    }

    bandwidthFile.close()
  }
}
