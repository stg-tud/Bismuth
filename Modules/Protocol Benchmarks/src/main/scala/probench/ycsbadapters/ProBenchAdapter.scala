package probench.ycsbadapters

import channels.{Abort, ConcurrencyHelper, NioTCP}
import probench.cli.addRetryingLatentConnection
import probench.clients.ProBenchClient
import probench.ycsbadapters.ProBenchAdapterConnectionPool.syncClient
import rdts.base.Uid
import site.ycsb.{ByteIterator, DB, Status, StringByteIterator}

import java.net.InetSocketAddress
import java.util.concurrent.Executors
import java.util.{HashMap, Map, Properties, Set, Vector}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

object ProBenchAdapterConnectionPool {
  private val receiveEC: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val sendEC: ExecutionContext    = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val pbClient                    = ProBenchClient(name = Uid.gen(), logTimings = false)
  private val nioTCP: NioTCP              = NioTCP(ConcurrencyHelper.makeExecutionContext(false))
  private val abort: Abort                = Abort()

  @volatile var connections: scala.collection.immutable.Set[(String, Int)] = scala.collection.immutable.Set.empty

  pbClient.printResults = false
  receiveEC.execute(() => nioTCP.loopSelection(abort))

  val counter = new java.util.concurrent.atomic.AtomicInteger(0)

//  (new java.util.Timer()).scheduleAtFixedRate(() => pprint.pprintln(pbClient.currentState), 0, 1000)

  def syncClient[A](f: ProBenchClient => Future[A]): Future[A] = Future {
    val count = counter.incrementAndGet()
    // println(s"syncClient: ${Thread.currentThread().getName} ${count} scheduling task")
    val res = f(pbClient)
    // res.onComplete(_ => println(s"syncClient: ${Thread.currentThread().getName} ${count} finished task "))
    // println(s"syncClient: ${Thread.currentThread().getName} ${count} complete schedule")
    res
  }(using sendEC).flatten

  def addConnection(ip: String, port: Int): Unit = synchronized {

    if connections.contains(ip, port) then ()
    else
        println(s"adding connection to $ip:$port")
        addRetryingLatentConnection(
          pbClient.writeDataManager,
          nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(ip, port))),
          1000,
          10
        )
        addRetryingLatentConnection(
          pbClient.readDataManager,
          nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(ip, port - 1))),
          1000,
          10
        )
        connections = connections + (ip -> port)

  }

}

class ProBenchAdapter extends DB {

  private var operationTimeout: FiniteDuration   = 1.seconds
  private var endpoints: Array[(String, String)] = Array.empty
  private var currentEndpointIndex: Int          = -1
  private val initialBackoff: Long = 0
  private var backoff: Long = initialBackoff

  private def connectToNextEndpoint(): Boolean =  {
    if currentEndpointIndex == 0 && endpoints.length == 1 then {
      println("no more endpoints to try. All known endpoints have failed")
      return false
    } else if currentEndpointIndex + 1 < endpoints.length then
      currentEndpointIndex = currentEndpointIndex + 1
    else
      currentEndpointIndex = 0 // start from beginning

    val (ip, port) = endpoints(currentEndpointIndex)
    println(s"ensuring connection to $ip:$port")
    ProBenchAdapterConnectionPool.addConnection(ip, Integer.parseInt(port))
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
    if props.stringPropertyNames.contains("pb.op-timeout") then
      operationTimeout = Integer.parseInt(props.getProperty("pb.op-timeout")).seconds
    endpoints = props.getProperty("pb.endpoints").split(" ").map(e =>
        val s = e.split(":")
        (s(0), s(1))
    )
    connectToNextEndpoint()

    println(s"Hello from pb adapter! $this")
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = {
    val v = valsToString(values)
    try
        val f = syncClient(_.writeWithResult(key, v))
        Await.ready(f, operationTimeout)
        Status.OK
    catch
        case exception: concurrent.TimeoutException =>
          println(s"failed to write sth")
          exception.printStackTrace()
          connectToNextEndpoint(): Unit  // try with next endpoint
          Status.ERROR
  }

  override def read(table: String, key: String, fields: Set[String], result: Map[String, ByteIterator]): Status = {
    try
        val f = syncClient(_.readWithResult(key).map(res =>
          result.put("result", StringByteIterator(res))
        ))
        Await.ready(f, operationTimeout)
        backoff = initialBackoff
        Status.OK
    catch
        case exception: concurrent.TimeoutException =>
          println(s"failed to read sth")
          exception.printStackTrace()
          connectToNextEndpoint(): Unit // try with next endpoint
          Thread.sleep(backoff)
          backoff = backoff + 500
          Status.ERROR
  }

  override def scan(
      table: String,
      startkey: String,
      recordcount: Int,
      fields: Set[String],
      result: Vector[HashMap[String, ByteIterator]]
  ): Status =
    Status.NOT_IMPLEMENTED

  override def update(table: String, key: String, values: Map[String, ByteIterator]): Status =
    insert(table, key, values)
}
