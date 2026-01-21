package probench.ycsbadapters

import channels.{Abort, ConcurrencyHelper, NioTCP}
import probench.cli.addRetryingLatentConnection
import probench.clients.ProBenchClient
import rdts.base.Uid
import site.ycsb.{ByteIterator, DB, Status, StringByteIterator}

import java.net.InetSocketAddress
import java.util.concurrent.{ExecutorService, Executors}
import java.util.{HashMap, Map, Properties, Set, Vector}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*

class ProBenchAdapter extends DB {

  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)
  private val pbClient                  = ProBenchClient(name = Uid.gen(), logTimings = false)
  private val nioTCP: NioTCP            = NioTCP(ConcurrencyHelper.makePooledExecutor(4))
  private val abort: Abort              = Abort()

  val operationTimeout: FiniteDuration = 20.seconds

  private def valsToString(values: Map[String, ByteIterator]) = {
    val a = StringByteIterator.getStringMap(values)
    a.asScala.mkString(";")
  }

  override def delete(table: String, key: String): Status =
    Status.NOT_IMPLEMENTED

  override def init(): Unit = {
    val props: Properties = getProperties
    val endpoints         = props.getProperty("pb.endpoints").split(" ").map(e =>
        val s = e.split(":")
        (s(0), s(1))
    )
    pbClient.printResults = false

    ec.execute(() => nioTCP.loopSelection(abort))

    endpoints.foreach { (ip, port) =>
      println(s"adding connection to $ip:$port")
      addRetryingLatentConnection(
        pbClient.dataManager,
        nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(ip, Integer.parseInt(port)))),
        1000,
        10
      )
    }

    println(s"Hello from pb adapter! $this")
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = {
    val v = valsToString(values)
    try
        val f = pbClient.writeWithResult(key, v)
        Await.ready(f, operationTimeout)
        Status.OK
    catch
        case exception: concurrent.TimeoutException =>
          println(s"failed to write sth")
          exception.printStackTrace()
          Status.ERROR
  }

  override def read(table: String, key: String, fields: Set[String], result: Map[String, ByteIterator]): Status = {
    try
        val f = pbClient.readWithResult(key).map(res =>
          result.put("result", StringByteIterator(res))
        )
        Await.ready(f, operationTimeout)
        Status.OK
    catch
        case exception: concurrent.TimeoutException =>
          println(s"failed to read sth")
          exception.printStackTrace()
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
