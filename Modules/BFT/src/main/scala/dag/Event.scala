package dag

//import com.github.plokhotnyuk.jsoniter_scala.core.*
//import com.github.plokhotnyuk.jsoniter_scala.macros.*

import java.security.MessageDigest


case class Event[T](
                     id: String,
                     content: Option[T],
                     author: Array[Byte],
                     dependencies: Set[String],
                     signature: String
                   ):

  private val HashAlgorithm = "SHA3-512"

  def calculateHash: String =
    val bytes = s"${content.toString}$author${dependencies.toString()}$signature".getBytes
    MessageDigest.getInstance(HashAlgorithm).digest(bytes).mkString("Array(", ", ", ")")


object Event:

  def apply[T](content: Option[T], author: Array[Byte], dependencies: Set[String]): Event[T] =
    val signature = ""// TODO: sign data

    val tmp = Event("", content, author, dependencies, signature)
    val id = tmp.calculateHash

    Event(id, content, author, dependencies, signature)
