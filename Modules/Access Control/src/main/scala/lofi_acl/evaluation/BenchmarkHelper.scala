package lofi_acl.evaluation

import scala.annotation.tailrec
import scala.util.Random

object BenchmarkHelper {

  def dummy(using random: Random): String = random.alphanumeric.take(20).mkString("")

  def pickOne[V](set: Set[V])(using random: Random): V = set.drop(random.nextInt(set.size)).head

  @tailrec
  def retryUntilSuccess[T](action: => T): T =
    try
      action
    catch {
      case _: Throwable => retryUntilSuccess(action)
    }

}
