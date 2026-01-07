package probench.ycsbadapters

import channels.{Abort, NioTCP}
import probench.cli.addRetryingLatentConnection
import probench.clients.ProBenchClient
import rdts.base.Uid
import site.ycsb.{ByteIterator, DB, Status, StringByteIterator}

import java.net.InetSocketAddress
import java.util.concurrent.{ExecutorService, Executors}
import java.util.{HashMap, Map, Properties, Set, Vector}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls

class ProBenchAdapter extends DB {

  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)
  private val pbClient                  = ProBenchClient(name = Uid.gen(), logTimings = false)
  private var nioTCP: NioTCP            = null
  private var abort: Abort              = null

  private def valsToString(values: Map[String, ByteIterator]) = {
    val a = StringByteIterator.getStringMap(values)
    a.asScala.mkString(";")
  }

  override def cleanup(): Unit = {
    abort.closeRequest = true
    executor.shutdownNow(): Unit
  }

  override def delete(table: String, key: String): Status =
    Status.NOT_IMPLEMENTED

  override def init(): Unit = {
    val props: Properties = getProperties
    val ip                = props.getProperty("pb.ip")
    val port              = Integer.parseInt(props.getProperty("pb.port"))
    pbClient.printResults = false

    nioTCP = NioTCP()
    abort = Abort()
    ec.execute(() => nioTCP.loopSelection(abort))

    addRetryingLatentConnection(
      pbClient.dataManager,
      nioTCP.connect(nioTCP.defaultSocketChannel(InetSocketAddress(ip, port))),
      1000,
      10
    )

    println("Hello from pb adapter!")
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = {
    val v = valsToString(values)
    try
        // println(s"writing ($k,$v)")
        pbClient.write(key, v)
        Status.OK
    catch
        case exception =>
//          println(exception.toString)
          Status.ERROR
  }

  override def read(table: String, key: String, fields: Set[String], result: Map[String, ByteIterator]): Status = {
    try
        pbClient.read(key)
        Status.OK
    catch
        case exception =>
//          println(exception.toString)
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
