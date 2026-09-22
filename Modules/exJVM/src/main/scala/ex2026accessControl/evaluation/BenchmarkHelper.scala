package ex2026accessControl.evaluation

import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026accessControl.evaluation.BenchmarkRdt.given
import ex2026accessControl.travelplanner.TravelPlan
import rdts.base.LocalUid
import rdts.datatypes.PosNegCounter
import rdts.filters.PermissionTree
import rdts.syntax.deltalens.*

import scala.annotation.tailrec
import scala.collection.mutable
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

  def generateReplicaIds(numReplicas: Int): Array[PrivateIdentity] = {
    require(numReplicas >= 1)
    0.until(numReplicas).map(_ => IdentityFactory.createNewIdentity).toArray
  }

}
