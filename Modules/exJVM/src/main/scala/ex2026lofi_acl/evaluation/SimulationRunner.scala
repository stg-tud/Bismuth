package ex2026lofi_acl.evaluation

import cats.implicits.*
import com.monovore.decline.*
import SimulationBenchmark.start

object SimulationRunnerArguments {
  case class Config(
      replicas: Int,
      deltas: Int,
      repetitions: Int,
      warmup: Int,
      bind: String
  )

  object ConfigOpts:
      val replicas: Opts[Int] = Opts.option[Int]("replicas", help = "Number of replicas").withDefault(10)

      val deltas: Opts[Int] =
        Opts.option[Int]("deltas", help = "Number of deltas in total (distributed among replicas)").withDefault(1)

      val repetitions: Opts[Int] = Opts.option[Int]("repetitions", help = "Number of runs").withDefault(200)

      val warmup: Opts[Int] = Opts.option[Int]("warmup", help = "Number of warmup iterations").withDefault(10)

      val bind: Opts[String] = Opts.option[String](
        "bind",
        help =
          "Pattern for IP addresses the replicas bind to. '*' is replaced with the index of the replica (starts at 1)."
      ).withDefault("127.0.0.1")

      val config: Opts[Config] =
        (replicas, deltas, repetitions, warmup, bind).mapN(Config.apply)

}

object SimulationRunner extends CommandApp(
      name = "simulation",
      header = "Runs the simulation benchmarks",
      main = SimulationRunnerArguments.ConfigOpts.config.map: config =>
          if config.warmup > 0 then
              println(s"Performing ${config.warmup} warmup runs")
              start(config.deltas, config.replicas, config.warmup, config.bind): Unit
              println("Warmup complete")

          val results: Seq[String] = start(config.deltas, config.replicas, config.repetitions, config.bind)

          println("replicas,num_deltas_per_replica,centralized,enforcing,runtime_ns")
          println(results.mkString("\n"))
    )
