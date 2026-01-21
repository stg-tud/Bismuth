package ex2025blockchain

import rdts.time.Dot

import java.security.MessageDigest
import scala.annotation.tailrec

case class Block[T](hash: String, previousHash: Option[String], data: T, dot: Dot, timestamp: Long)

object Block {

  private val target = "0"

  def apply[T](hash: String, previousHash: Option[String], data: T, dot: Dot): Block[T] =
    Block(hash, previousHash, data, dot, 0L)

  def apply[T](previousHash: String, data: T, dot: Dot, difficulty: Int): Block[T] = {
    val hash = mineHash(previousHash, data, dot, System.currentTimeMillis(), difficulty)
    Block(hash._1, Option(previousHash), data, dot, hash._2)
  }

  @tailrec
  final def mineHash[T](previousHash: String, data: T, dot: Dot, now: Long, difficulty: Int): (String, Long) = {
    val input     = s"$now$previousHash$data${dot.place}${dot.time}"
    val digest    = MessageDigest.getInstance("SHA-256")
    val hashBytes = digest.digest(input.getBytes("UTF-8"))
    val hash      = hashBytes.map("%02x".format(_)).mkString

    if hash.startsWith(target * difficulty) then (hash, now)
    else mineHash(previousHash, data, dot, System.currentTimeMillis(), difficulty)
  }
}
