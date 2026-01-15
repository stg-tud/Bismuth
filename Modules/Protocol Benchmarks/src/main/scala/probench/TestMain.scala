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
      "localhost:8121",
      "localhost:8131",
      "--initial-cluster-ids",
      "node1 node2 node3"
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
      "localhost:8131",
      "--initial-cluster-ids",
      "node1 node2 node3"
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
      "node1 node2 node3"
    ).toArray)

  }
}
