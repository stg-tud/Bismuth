package probench

import channels.*
import de.rmgk.options.*
import de.rmgk.options.Result.{Err, Ok}
import probench.KnowledgeGroups.*
import probench.clients.*
import rdts.base.Uid
import rdts.protocols.knowledgeGroups.MultiPaxos
import replication.{DeltaDissemination, DeltaStorage}

import java.net.{DatagramSocket, InetSocketAddress}
import java.util.Timer
import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.unused
import scala.concurrent.ExecutionContext
import scala.util.matching.Regex
import scala.util.{Failure, Success}

object cli {

  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)

  def addRetryingLatentConnection(
      dataManager: DeltaDissemination[?],
      connection: LatentConnection[MessageBuffer],
      delay: Long,
      tries: Int
  ): Unit = {

    dataManager.prepareBinaryConnection(connection).run {
      case Success(_)  => ()
      case Failure(ex) =>
        println(s"Failed to connect to $connection, retrying in $delay ms, retries: $tries")
        if tries > 0 then
            Thread.sleep(delay)
            addRetryingLatentConnection(dataManager, connection, delay, tries - 1)
        else
            throw ex
    }

  }

  def main(args: Array[String]): Unit = {

    val clientPort = named[Int]("--listen-client-port", "")
    val peerPort   = named[Int]("--listen-peer-port", "")

    val reporting = flag("--reporting", "enable reporting")

    val ipAndPort = """(.+):(\d+)""".r

    given ipAndPortParser: ArgumentValueParser[(String, Int)] with
        override def parse(args: List[String]): Result[(String, Int)] =
            import scala.language.unsafeNulls
            args match {
              case ipAndPort(ip, port) :: rest => Ok((ip, Integer.parseInt(port)), rest)
              case _                           => Err("not a valid ip:port")
            }

        def descriptor: de.rmgk.options.Descriptor = Descriptor("ip:port", "ip:port pair")
    end ipAndPortParser

    given uidParser: ArgumentValueParser[Uid] with
        override def parse(args: List[String]): Result[Uid] =
          args match {
            case string :: rest => Result.Ok(Uid.predefined(string), rest)
            case _              => Result.Err("not a valid uid", descriptor)
          }

        override def descriptor: Descriptor = Descriptor("uid", "uid")

    end uidParser

    given benchmarkModeParser: ArgumentValueParser[BenchmarkMode] with
        override def parse(args: List[String]): Result[BenchmarkMode] =
          args match {
            case "fixed" :: rest => Result.Ok(BenchmarkMode.Fixed, rest)
            case "timed" :: rest => Result.Ok(BenchmarkMode.Timed, rest)
            case _               => Result.Err(s"not a valid benchmark mode: $args", descriptor)
          }

        override def descriptor: Descriptor = Descriptor("mode", "fixed|timed")

    end benchmarkModeParser

    given benchmarkOpTypeParser: ArgumentValueParser[BenchmarkOpType] with
        override def parse(args: List[String]): Result[BenchmarkOpType] =
          args match {
            case "read" :: rest  => Result.Ok(BenchmarkOpType.Read, rest)
            case "write" :: rest => Result.Ok(BenchmarkOpType.Write, rest)
            case "mixed" :: rest => Result.Ok(BenchmarkOpType.Mixed, rest)
            case _               => Result.Err("not a valid benchmark opType", descriptor)
          }

        override def descriptor: Descriptor = Descriptor("opType", "read|write|mixed")

    end benchmarkOpTypeParser

    given deltaStorageTypeParser: ArgumentValueParser[DeltaStorage.Type] with
        val discarding: Regex = """discarding\((\d+)\)""".r
        val merging: Regex    = """merging\((\d+)\)""".r

        override def parse(args: List[String]): Result[DeltaStorage.Type] =
            import scala.language.unsafeNulls
            args match {
              case discarding(maxSize) :: rest => Result.Ok(DeltaStorage.Type.Discarding(maxSize.toInt), rest)
              case "state" :: rest             => Result.Ok(DeltaStorage.Type.State, rest)
              case "keep-all" :: rest          => Result.Ok(DeltaStorage.Type.KeepAll, rest)
              case merging(blockSize) :: rest  => Result.Ok(DeltaStorage.Type.Merging(blockSize.toInt), rest)
              case _                           => Result.Err("not a valid delta storage type", descriptor)
            }

        override def descriptor: Descriptor =
          Descriptor("delta-storage-type", "discarding(<max-size>), state, keep-all, merging(<block-size>)")
    end deltaStorageTypeParser

    given booleanParser: ArgumentValueParser[Boolean] with
        override def parse(args: List[String]): Result[Boolean] =
          args match {
            case "true" :: rest  => Result.Ok(true, rest)
            case "false" :: rest => Result.Ok(false, rest)
            case _               => Result.Err("not a valid boolean", descriptor)
          }

        override def descriptor: Descriptor = Descriptor("boolean", "true|false")
    end booleanParser

    class TupleArgumentValueParser[T](@unused /* TODO: this seems wrong … */ preselect: String => Boolean)(using
        avp: ArgumentValueParser[T]
    ) extends ArgumentValueParser[(T, T)] {
      override def parse(args: List[String]): Result[(T, T)] =
        avp.parse(args) match
            case Result.Ok(first, remaining) =>
              avp.parse(remaining) match
                  case Result.Ok(second, remaining) => Result.Ok((first, second), remaining)
                  case _: Result.Err                => Result.Err("not a valid tuple", descriptor)
            case _: Result.Err => Result.Err("not a valid tuple", descriptor)

      override def descriptor: Descriptor = avp.descriptor.mapSpec(s => s"$s $s")
    }

    given [T](using avp: ArgumentValueParser[T]): ArgumentValueParser[(T, T)] =
      TupleArgumentValueParser[T](arg => !arg.startsWith("--"))

    def socketPath(host: String, port: Int) =
//      val p = Path.of(s"target/sockets/$name")
//      Files.createDirectories(p.getParent)
//      p.toFile.deleteOnExit()
//      UnixDomainSocketAddress.of(p)

      InetSocketAddress(host, port)

    val cluster           = named[List[(String, Int)]]("--cluster", "")
    val initialClusterIds = named[List[Uid]]("--initial-cluster-ids", "")
    val clientNode        = named[(String, Int)]("--node", "<ip:port>")
    val name              = named[Uid]("--name", "", Uid.gen())
    val endpoints         = named[List[String]]("--endpoints", "")
    val deltaStorageType  = named[DeltaStorage.Type]("--delta-storage-type", "", DeltaStorage.Type.Merging(2000))
    val logTimings        = named[Boolean]("--log-timings", "true|false", true)
    val commitReads       = named[Boolean]("--commit-reads", "true|false", false)

    /*
    fixed opType warmup measurement min max
    timed opType warmup measurement min max blockSize
     */

    val mode         = named[BenchmarkMode]("--mode", "mode for the benchmark")
    val opType       = named[BenchmarkOpType]("--op-type", "opType for the benchmark")
    val warmup       = named[Int]("--warmup", "warmup period/operations for the benchmark in seconds")
    val measurement  = named[Int]("--measurement", "measurement period/operations for the benchmark in seconds")
    val kvRange      = named[(Int, Int)]("--kv-range", "min/max key/value index", (1000, 1999))
    val blockSize    = named[Int]("--block-size", "block size for timed benchmarks")
    val timeout      = named[Long]("--timeout", "timeout before new leader is elected in miliseconds", 1000)
    val systemConfig = named[String]("--system-config", "PRDT system config")

    val argparse = composedParser {

      alternatives(
        subcommand("multipaxos-node", "starts multiPaxos node with configurable knowledge group") {
          val uid          = name.value
          val config       = systemConfig.value
          val configObject = config match {
            case "clientServer"         => KnowledgeGroups.clientServer
            case "compartmentalization" => KnowledgeGroups.compartmentalized
            case "occamsRazor"          => KnowledgeGroups.occamsRazor
            case _                      => throw Exception(s"invalid system config $config")
          }
          val node =
            MultiPaxosReplica(
              id = uid,
              participants = initialClusterIds.value.toSet,
              systemConfig = configObject,
              state = MultiPaxos()
            )

          // setup connections
          val reporter = if reporting.value then ChannelTrafficReporter() else null
          val nioTCP   = NioTCP(ConcurrencyHelper.makePooledExecutor(), reporter)
          ec.execute(() => nioTCP.loopSelection(Abort()))

          val peerPortVal = peerPort.value

          // setup connections
          config match
              case "clientServer" =>
                if uid == Uid.predefined("leader") then {
                  println("setting up connections for leader")
                  // client leader
                  node.dataManagers(Set(
                    client,
                    leader
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                  // leader followers
                  node.dataManagers(Set(
                    leader,
                    follower1,
                    follower2,
                    follower3,
                    follower4
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 1
                  ))))
                }

                println(s"data managers: ${node.dataManagers}")

                cluster.value.foreach { (host, port) =>
                  println(s"Connecting to $host:$port")
                  addRetryingLatentConnection(
                    node.dataManagers(Set(leader, follower1, follower2, follower3, follower4)),
                    nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
                    1000,
                    10
                  )
                }
              case "occamsRazor" =>
                if uid == Uid.predefined("leader") then {
                  println("setting up connections for leader")
                  // client leader
                  node.dataManagers(Set(
                    client,
                    leader
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                  // leader followers
                  node.dataManagers(Set(
                    leader,
                    follower1,
                    follower2,
                    follower3,
                    follower4
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 1
                  ))))
                  // client leader followers
                  node.dataManagers(Set(
                    client,
                    leader,
                    follower1,
                    follower2,
                    follower3,
                    follower4
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 2
                  ))))
                }
                if uid == Uid.predefined("follower1") || uid == Uid.predefined("follower2") || uid == Uid.predefined(
                      "follower3"
                    ) || uid == Uid.predefined("follower4")
                then {
                  println(s"setting up connections for $uid")
                  // followers client
                  node.dataManagers(Set(
                    client,
                    leader,
                    follower1,
                    follower2,
                    follower3,
                    follower4
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                }

                println(s"data managers: ${node.dataManagers}")

                cluster.value.foreach { (host, port) =>
                  println(s"Connecting to $host:$port")
                  addRetryingLatentConnection(
                    node.dataManagers(Set(leader, follower1, follower2, follower3, follower4)),
                    nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
                    1000,
                    10
                  )
                  println(s"Connecting to $host:${port + 1}")
                  addRetryingLatentConnection(
                    node.dataManagers(Set(client, leader, follower1, follower2, follower3, follower4)),
                    nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port + 1))),
                    1000,
                    10
                  )
                }
              case "compartmentalization" =>
                // prepare server connections
                if uid == Uid.predefined("leader") then {
                  println("setting up connections for leader")
                  // client leader
                  node.dataManagers(Set(
                    client,
                    leader
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                  // leader proxy1
                  node.dataManagers(Set(
                    leader,
                    proxy1
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 1
                  ))))
                  // leader proxy2
                  node.dataManagers(Set(
                    leader,
                    proxy2
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 2
                  ))))
                }
                if uid == Uid.predefined("proxy1") then {
                  println("setting up connections for proxy1")
                  // proxy1 client
                  node.dataManagers(Set(
                    proxy1,
                    proxy2,
                    client
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                  // proxy1 follower1 follower3
                  node.dataManagers(Set(
                    proxy1,
                    follower1,
                    follower3
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 1
                  ))))
                }
                if uid == Uid.predefined("proxy2") then {
                  println("setting up connections for proxy2")
                  // proxy2 client
                  node.dataManagers(Set(
                    proxy1,
                    proxy2,
                    client
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal
                  ))))
                  // proxy2 follower2 follower4
                  node.dataManagers(Set(
                    proxy2,
                    follower2,
                    follower4
                  )).addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
                    "0",
                    peerPortVal + 1
                  ))))
                }

                println(s"data managers: ${node.dataManagers}")
                // connect as client to existing server connections
                if uid == Uid.predefined("proxy1") then {
                  cluster.value.foreach { (host, port) =>
                    println(s"Connecting to $host:$port")
                    addRetryingLatentConnection(
                      node.dataManagers(Set(leader, proxy1)),
                      nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
                      1000,
                      10
                    )
                  }
                }
                if uid == Uid.predefined("proxy2") then {
                  cluster.value.foreach { (host, port) =>
                    println(s"Connecting to $host:$port")
                    addRetryingLatentConnection(
                      node.dataManagers(Set(leader, proxy2)),
                      nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port + 1))),
                      1000,
                      10
                    )
                  }
                }
                if (uid == Uid.predefined("follower1")) || (uid == Uid.predefined("follower3")) then {
                  cluster.value.foreach { (host, port) =>
                    println(s"Connecting to $host:$port")
                    addRetryingLatentConnection(
                      node.dataManagers(Set(proxy1, follower1, follower3)),
                      nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
                      1000,
                      10
                    )
                  }
                }
                if (uid == Uid.predefined("follower2")) || (uid == Uid.predefined("follower4")) then {
                  cluster.value.foreach { (host, port) =>
                    println(s"Connecting to $host:$port")
                    addRetryingLatentConnection(
                      node.dataManagers(Set(proxy2, follower2, follower4)),
                      nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
                      1000,
                      10
                    )
                  }
                }
              case _ => throw Exception(s"need to provide a valid system config, got: $config")

          if uid == Uid.predefined("leader") then {
            val timer = Timer()
            timer.schedule(
              () => { node.startLeaderElection(); timer.cancel() },
              10000,
              10000
            )
          }
        },
        subcommand("node", "starts a cluster node") {
          val node =
            KeyValueReplica(
              name.value,
              initialClusterIds.value.toSet,
              deltaStorageType = deltaStorageType.value,
              timeoutThreshold = timeout.value,
              commitReads = commitReads.value
            )

          val reporter = if reporting.value then ChannelTrafficReporter() else null

          val nioTCP = NioTCP(ConcurrencyHelper.makePooledExecutor(), reporter)
          ec.execute(() => nioTCP.loopSelection(Abort()))

          val clientPortVal = clientPort.value
          node.client.dataManagerWrite.addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            clientPortVal
          ))))
          node.client.dataManagerRead.addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            clientPortVal - 1
          ))))

          val peerPortVal = peerPort.value

          node.cluster.dataManager.addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            peerPortVal
          ))))
          node.connInf.dataManager.addBinaryConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            peerPortVal + 1
          ))))

//          Timer().schedule(() => node.cluster.dataManager.pingAll(), 1000, 1000)
          if reporter != null then
              Timer().schedule(
                () => {
                  println(s"client ${node.uid}")
                  println(reporter.report())
                  reporter.reset()
                },
                1000,
                1000
              )
          Timer().schedule(
            () =>
              node.connInf.sendHeartbeat(): Unit,
            100,
            200
          )
          Timer().schedule(
            () =>
              node.connInf.checkLiveness(),
            1000,
            500
          )

          cluster.value.foreach { (host, port) =>
            println(s"Connecting to $host:${port + 1}")
            addRetryingLatentConnection(
              node.connInf.dataManager,
              nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port + 1))),
              1000,
              10
            )
            println(s"Connecting to $host:$port")
            addRetryingLatentConnection(
              node.cluster.dataManager,
              nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
              1000,
              10
            )
//            println(s"Connecting to $host:${port - 1}")
//            addRetryingLatentConnection(
//              node.client.dataManagerWrite,
//              nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port - 1))),
//              1000,
//              10
//            )
//            println(s"Connecting to $host:${port - 2}")
//            addRetryingLatentConnection(
//              node.client.dataManagerRead,
//              nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port - 2))),
//              1000,
//              10
//            )
          }
        },
        subcommand("udp-node", "starts a cluster node") {
          val node =
            KeyValueReplica(name.value, initialClusterIds.value.toSet, deltaStorageType = deltaStorageType.value)

          node.client.dataManagerWrite.addBinaryConnection(UDP.listen(() => new DatagramSocket(clientPort.value), ec))
          node.cluster.dataManager.addBinaryConnection(UDP.listen(() => new DatagramSocket(peerPort.value), ec))
          node.connInf.dataManager.addBinaryConnection(UDP.listen(() => new DatagramSocket(peerPort.value + 1), ec))

          Timer().schedule(() => node.cluster.dataManager.pingAll(), 1000, 1000)

          cluster.value.foreach { (ip, port) =>
            node.cluster.dataManager.addBinaryConnection(UDP.connect(
              InetSocketAddress(ip, port),
              () => new DatagramSocket(),
              ec
            ))
            node.connInf.dataManager.addBinaryConnection(UDP.connect(
              InetSocketAddress(ip, port + 1),
              () => new DatagramSocket(),
              ec
            ))
          }
        },
        subcommand("client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value, logTimings = logTimings.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP(ConcurrencyHelper.makePooledExecutor())
          val abort  = Abort()
          ec.execute(() => nioTCP.loopSelection(abort))

          addRetryingLatentConnection(
            client.writeDataManager,
            nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))),
            1000,
            10
          )

          ClientCLI(name.value, client).startCLI()

          abort.closeRequest = true
          executor.shutdownNow()
        },
        subcommand("udp-client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value, logTimings = logTimings.value)

          val (ip, port) = clientNode.value

          client.writeDataManager.addBinaryConnection(UDP.connect(
            InetSocketAddress(ip, port),
            () => new DatagramSocket(),
            ec
          ))

          ClientCLI(name.value, client).startCLI()
          executor.shutdownNow()
        },
        subcommand("benchmark-client", "starts a benchmark client") {
          val client = ProBenchClient(name.value, logTimings = logTimings.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP(ConcurrencyHelper.makeExecutionContext(false))
          val abort  = Abort()
          ec.execute(() => nioTCP.loopSelection(abort))

          addRetryingLatentConnection(
            client.writeDataManager,
            nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))),
            1000,
            10
          )

          mode.value match
              case BenchmarkMode.Timed =>
                val (min, max) = kvRange.value
                client.benchmarkTimed(
                  mode = opType.value,
                  warmup = warmup.value,
                  measurement = measurement.value,
                  min = min,
                  max = max,
                  blockSize = blockSize.value
                )
              case BenchmarkMode.Fixed =>
                val (min, max) = kvRange.value
                client.benchmarkFixed(
                  mode = opType.value,
                  warmup = warmup.value,
                  measurement = measurement.value,
                  min = min,
                  max = max
                )

          abort.closeRequest = true
          executor.shutdownNow()
        },
        subcommand("etcd-benchmark", "starts a benchmark client") {
          val client = EtcdClient(name.value, endpoints.value, logTimings.value)

          mode.value match
              case BenchmarkMode.Timed =>
                val (min, max) = kvRange.value
                client.benchmarkTimed(
                  mode = opType.value,
                  warmup = warmup.value,
                  measurement = measurement.value,
                  min = min,
                  max = max,
                  blockSize = blockSize.value
                )
              case BenchmarkMode.Fixed =>
                val (min, max) = kvRange.value
                client.benchmarkFixed(
                  mode = opType.value,
                  warmup = warmup.value,
                  measurement = measurement.value,
                  min = min,
                  max = max
                )

          executor.shutdownNow()
        },
        subcommand("etcd-client", "starts a client to interact with an etcd cluster") {
          val client = EtcdClient(name.value, endpoints.value, logTimings.value)

          ClientCLI(name.value, client).startCLI()

          executor.shutdownNow()
        },
      )
    }

    parse(args.toList)(argparse)
    ()
  }

}
