package probench

object TestMain {
  def main(args: Array[String]): Unit = {

    cli.main(List(
      "node",
      "--name",
      "node1",
      "--listen-client-port",
      "8110",
      "--listen-peer-port",
      "8111",
      "--cluster",
      "localhost:8111",
      "--initial-cluster-ids",
//      "node1"
      "node1 node2 node3 node4 node5"
    ).toArray)


    cli.main(List(
      "node",
      "--name",
      "node2",
      "--listen-client-port",
      "8120",
      "--listen-peer-port",
      "8121",
      "--cluster",
      "localhost:8111",
      "localhost:8121",
      "--initial-cluster-ids",
      "node1 node2 node3 node4 node5"
    ).toArray)

    cli.main(List(
      "node",
      "--name",
      "node3",
      "--listen-client-port",
      "8130",
      "--listen-peer-port",
      "8131",
      "--cluster",
      "localhost:8111",
      "localhost:8121",
      "localhost:8131",
      "--initial-cluster-ids",
      "node1 node2 node3 node4 node5"
    ).toArray)

    cli.main(List(
      "node",
      "--name",
      "node4",
      "--listen-client-port",
      "8140",
      "--listen-peer-port",
      "8141",
      "--cluster",
      "localhost:8111",
      "localhost:8121",
      "localhost:8131",
      "localhost:8141",
      "--initial-cluster-ids",
      "node1 node2 node3 node4 node5"
    ).toArray)

    cli.main(List(
      "node",
      "--name",
      "node5",
      "--listen-client-port",
      "8150",
      "--listen-peer-port",
      "8151",
      "--cluster",
      "localhost:8111",
      "localhost:8121",
      "localhost:8131",
      "localhost:8141",
      "localhost:8151",
      "--initial-cluster-ids",
      "node1 node2 node3 node4 node5"
    ).toArray)

    while(true) {

    }
  }
}
