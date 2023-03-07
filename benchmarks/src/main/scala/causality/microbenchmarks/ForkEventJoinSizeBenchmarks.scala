package de.tu_darmstadt.stg.daimpl
package causality.microbenchmarks

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
        Paths.get(s"./results/num-replicas-${program.generationParams}-${program.programLength}.csv")
      )
    )

    val runner = new ProgramRunnerWithSerialization(program, program.vcReplicas)
    while (runner.commandIndex < program.programLength) {
      runner.step()
      bandwidthFile.println(s"${runner.commandIndex},${runner.bandwidthUsed}")
      numReplicasFile.println(s"${runner.commandIndex},${runner.numReplicas}")
    }

    bandwidthFile.close()
    numReplicasFile.close()
  }

  @Benchmark
  def sizeBenchmarkIntervalTreeClockWithSerialization(program: ForkEventJoinProgram): Unit = {
    val bandwidthFile = new PrintWriter(
      Files.newOutputStream(
        Paths.get(s"./results/itc-bandwidth-${program.generationParams}-${program.programLength}.csv")
      )
    )

    val runner = new ProgramRunnerWithSerialization(program, program.itcReplicas)
    while (runner.commandIndex < program.programLength) {
      runner.step()
      bandwidthFile.println(s"${runner.commandIndex},${runner.bandwidthUsed}")
    }

    bandwidthFile.close()
  }
}
