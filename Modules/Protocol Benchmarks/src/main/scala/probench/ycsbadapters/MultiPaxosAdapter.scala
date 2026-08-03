package probench.ycsbadapters

import channels.{Abort, ConcurrencyHelper, NioTCP}
import probench.KnowledgeGroups.*
import probench.cli.addRetryingLatentConnection
import probench.{KnowledgeGroups, MultiPaxosReplica}
import rdts.base.Uid
import rdts.protocols.knowledgeGroups.MultiPaxos
import site.ycsb.{ByteIterator, DB, Status, StringByteIterator}

import java.net.InetSocketAddress
import java.util.concurrent.Executors
import java.util.{HashMap, Map, Properties, Vector}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import MultiPaxosAdapterConnectionPool.{multiPaxosReplica, syncClient}

object MultiPaxosAdapterConnectionPool {
  private val receiveEC: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val sendEC: ExecutionContext    = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val nioTCP: NioTCP              = NioTCP(ConcurrencyHelper.makeExecutionContext(false))
  private val abort: Abort                = Abort()

  var multiPaxosReplica: MultiPaxosReplica | Null = null

  @volatile var connections: scala.collection.immutable.Set[(String, Int)] = scala.collection.immutable.Set.empty

  receiveEC.execute(() => nioTCP.loopSelection(abort))

  val counter = new java.util.concurrent.atomic.AtomicInteger(0)

//  (new java.util.Timer()).scheduleAtFixedRate(() => pprint.pprintln(pbClient.currentState), 0, 1000)

  def syncClient[A](f: MultiPaxosReplica => Future[A]): Future[A] = Future {
    val count = counter.incrementAndGet()
    // println(s"syncClient: ${Thread.currentThread().getName} ${count} scheduling task")
    val res = f(multiPaxosReplica)
    // res.onComplete(_ => println(s"syncClient: ${Thread.currentThread().getName} ${count} finished task "))
    // println(s"syncClient: ${Thread.currentThread().getName} ${count} complete schedule")
    res
  }(using sendEC).flatten

  def addConnection(ip: String, port: Int): Unit = synchronized {

    if connections.contains(ip, port) then ()
    else
        println(s"adding connection to $ip:$port")
        // todo add other collections for other knowledge groups
        addRetryingLatentConnection(
          multiPaxosReplica.dataManagers(Set(client, leader)),
          nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(ip, port))),
          1000,
          10
        )
        connections = connections + (ip -> port)

  }

}

class MultiPaxosAdapter extends DB {

  private var operationTimeout: FiniteDuration   = 1.seconds
  private var endpoints: Array[(String, String)] = Array.empty

  private def connectEndpoints(): Boolean = {
    if multiPaxosReplica.systemConfig == KnowledgeGroups.occamsRazor then
        endpoints.foreach((ip, port) =>
            println(s"ensuring connection to $ip:$port")
            MultiPaxosAdapterConnectionPool.addConnection(ip, Integer.parseInt(port))
        )
    else if multiPaxosReplica.systemConfig == KnowledgeGroups.compartmentalized then
        endpoints.take(3).foreach((ip, port) =>
            println(s"ensuring connection to $ip:$port")
            MultiPaxosAdapterConnectionPool.addConnection(ip, Integer.parseInt(port))
        )
    else
        val (ip, port) = endpoints.head
        println(s"ensuring connection to $ip:$port")
        MultiPaxosAdapterConnectionPool.addConnection(ip, Integer.parseInt(port))
    true
  }

  private def valsToString(values: Map[String, ByteIterator]) = {
    val a = StringByteIterator.getStringMap(values)
    a.asScala.mkString(";")
  }

  override def delete(table: String, key: String): Status =
    Status.NOT_IMPLEMENTED

  override def init(): Unit = {
    val props: Properties = getProperties
    if props.stringPropertyNames.contains("multipaxos.op-timeout") then
        operationTimeout = Integer.parseInt(props.getProperty("multipaxos.op-timeout")).milliseconds
    endpoints = props.getProperty("multipaxos.endpoints").split(" ").map(e =>
        val s = e.split(":")
        (s(0), s(1))
    )
    val systemConfig = props.getProperty("multipaxos.systemconfig")
    if MultiPaxosAdapterConnectionPool.multiPaxosReplica == null then {
      // val participants = props.getProperty("multipaxos.participants").split(" ").map(Uid.predefined).toSet
      val configObject = systemConfig match {
        case "clientServer"         => KnowledgeGroups.clientServer
        case "occamsRazor"          => KnowledgeGroups.occamsRazor
        case "compartmentalization" => KnowledgeGroups.compartmentalized
        case _                      => throw Exception(s"invalid system config: $systemConfig")
      }
      MultiPaxosAdapterConnectionPool.multiPaxosReplica = MultiPaxosReplica(
        id = Uid.predefined("client"),
        participants = Set(leader, follower1, follower2, follower3, follower4),
        systemConfig = configObject,
        state = MultiPaxos()
      )
    }
    connectEndpoints()

    println(s"Hello from MultiPaxos adapter! $this")
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = {
    val v  = valsToString(values)
    val id = Uid.gen()
    try
        val f = MultiPaxosAdapterConnectionPool.multiPaxosReplica.requestWithResult(id, v)
        Await.ready(f, operationTimeout)
        // println(s"(read/log/slots) ${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.read.size}/${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.log.size}/${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.slots.size}")
        Status.OK
    catch
        case exception: concurrent.TimeoutException =>
          println(
            s"failed to write id:$id\n$key\n${valsToString(values)}, (read/log/slots) ${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.read.size}/${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.log.size}/${MultiPaxosAdapterConnectionPool.multiPaxosReplica.state.slots.size}"
          )
          // exception.printStackTrace()
          Status.ERROR
  }

  override def read(
      table: String,
      key: String,
      fields: java.util.Set[String],
      result: Map[String, ByteIterator]
  ): Status =
    Status.NOT_IMPLEMENTED

  override def scan(
      table: String,
      startkey: String,
      recordcount: Int,
      fields: java.util.Set[String],
      result: Vector[HashMap[String, ByteIterator]]
  ): Status =
    Status.NOT_IMPLEMENTED

  override def update(table: String, key: String, values: Map[String, ByteIterator]): Status =
    insert(table, key, values)
}
