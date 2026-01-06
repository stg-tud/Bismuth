package probench.ycsbadapters

import io.etcd.jetcd
import io.etcd.jetcd.Client

import java.util.{HashMap, Map, Properties, Set, Vector}
import site.ycsb.{ByteIterator, DB, Status}

class EtcdClient extends DB {

  private var etcdClient: Client | Null = null

  override def cleanup(): Unit = ???

  override def delete(table: String, key: String): Status = ???

  override def init(): Unit = {
    val props: Properties = getProperties
    val endpoints         = props.getProperty("etcd.endpoints").split(" ")

    println("Hello World!")
    println(s"Endpoints: ${endpoints.mkString(" ")}")
    etcdClient = jetcd.Client.builder().endpoints(endpoints*).build()
  }

  override def insert(table: String, key: String, values: Map[String, ByteIterator]): Status = ???

  override def read(table: String, key: String, fields: Set[String], result: Map[String, ByteIterator]): Status = ???

  override def scan(
      table: String,
      startkey: String,
      recordcount: Int,
      fields: Set[String],
      result: Vector[HashMap[String, ByteIterator]]
  ): Status = ???

  override def update(table: String, key: String, values: Map[String, ByteIterator]): Status = ???

}
