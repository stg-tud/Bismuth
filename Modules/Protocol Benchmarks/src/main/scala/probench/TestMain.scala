package probench

import de.rmgk.script.*

import java.nio.file.Path

object TestMain {
  def main(args: Array[String]): Unit = {

    val nodes = 9
    val operationcount = "operationcount=100000"
    val threads = 20


    def clientPort(number: Int)   = 8100 + number * 10
    def peerPortPort(number: Int) = clientPort(number) + 1

    def initNode(number: Int, total: Int) = {
      (List(
        "node",
        "--name",
        s"node$number",
        "--listen-client-port",
        clientPort(number).toString,
        "--listen-peer-port",
        peerPortPort(number).toString,
        "--cluster"
      )
      ++ (1 to number).map(i => s"localhost:${peerPortPort(i)}").toList ++
      List(
        "--initial-cluster-ids",
        (1 to total).map(i => s"node$i").mkString(" ")
      ))
    }

    (1 to nodes).foreach { n =>
      println(s"starting node $n")

      cli.main(initNode(n, nodes).toArray)
    }

    val workdir = Path.of("target/ycsb").toAbsolutePath.normalize()

    process"rsync --info=progress2 --no-inc-recursive --archive --compress --delete root@46.224.98.103:ycsb-core.jar ${workdir}".run()
    process"rsync --info=progress2 --no-inc-recursive --archive --compress --delete root@46.224.98.103:workloads ${workdir}".run()

    process"""java -cp ycsb-core.jar:../jars/* site.ycsb.Client -db probench.ycsbadapters.ProBenchAdapter -P workloads/workloada -s -p pb.endpoints=localhost:8110  -p ${operationcount} -p recordcount=1000 -p measurementtype=histogram -threads ${threads}""".directory(Path.of("target/ycsb").toFile).run()

    println("done")

    ()

  }
}
