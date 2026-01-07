package probench.ycsbadapters

import io.etcd.jetcd
import io.etcd.jetcd.{ByteSequence, Client, KV}
import site.ycsb.{ByteIterator, DB, Status, StringByteIterator}

import java.nio.charset.StandardCharsets
import java.util.{HashMap, Map, Properties, Set, Vector}
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls

class EtcdAdapter extends DB {

  private var etcdClient: Client = null
  private var kvClient: KV       = null

  private def stringToByteSequence(key: String): ByteSequence =
    ByteSequence.from(key, StandardCharsets.UTF_8)

  private def valsToString(values: Map[String, ByteIterator]) = {
    val a = StringByteIterator.getStringMap(values)
    a.asScala.mkString(";")
  }

  override def cleanup(): Unit = {
    kvClient.close()
    etcdClient.close()
  }

  override def delete(table: String, key: String): Status =
    try
        kvClient.delete(stringToByteSequence(key)).get()
        Status.OK
    catch
        case exception =>
          println(exception.toString)
          Status.ERROR

  override def init(): Unit = {
    val props: Properties = getProperties
    val endpoints         = props.getProperty("etcd.endpoints").split(" ")

    etcdClient = jetcd.Client.builder().endpoints(endpoints*).build()
    kvClient = etcdClient.getKVClient
    println("Hello from etcd adapter!")
    println(s"Endpoints: ${endpoints.mkString(" ")}")
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = {
    val k = stringToByteSequence(key)
    val v = stringToByteSequence(valsToString(values))
    try
        kvClient.put(k, v).get()
        Status.OK
    catch
        case exception =>
          println(exception.toString)
          Status.ERROR
  }

  override def read(table: String, key: String, fields: Set[String], result: Map[String, ByteIterator]): Status = {
    val k = stringToByteSequence(key)
    try
        val res = kvClient.get(k).get().getKvs.get(0)
        result.put("result", StringByteIterator(res.getValue.toString))
        Status.OK
    catch
        case exception =>
          println(exception.toString)
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
