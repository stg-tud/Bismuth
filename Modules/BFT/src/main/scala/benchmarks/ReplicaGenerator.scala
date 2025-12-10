package benchmarks

import datatypes.{Counter, ORSet, Replica}
import scala.util.Random

object ReplicaGenerator:
  def generate[T, R <: Replica[T, R]](size: Int, diff: Float, replica1: R, replica2: R, n: Int = 1): (R, R) = {
    var r1  = replica1
    var r2  = replica2
    val rnd = new Random()
    val s = "A" * n * 1000

    val common = math.round(size * (1 - diff))
    for i <- Range(0, common - 1) do
      println(s"common: $i")
      r1 = performActionOnReplica(r1, s)
      r2 = performActionOnReplica(r2,s)

    println("merging")
    r1 = r1.merge(r2)
    r2 = r2.merge(r1)
    println("done merging")

    val diffsize = math.round(size * diff)
    for i <- Range(0, diffsize - 1) do
      println(s"diff: $i")
      r1 = performActionOnReplica(r1, s)
      r2 = performActionOnReplica(r2, s)

    (r1, r2)
  }

  private def performActionOnReplica[T, R <: Replica[T, R]](r: R, placeholder: String): R =
    val rnd = new Random()
    r match {
      case c @ Counter(hashDAG) => c.merge(c.add(rnd.nextInt()))
      case s @ ORSet(e, h)      =>
        val set = s.asInstanceOf[ORSet[String]]
        set.merge(set.add(placeholder)).asInstanceOf[R]
      case _ =>
        sys.error("Not implemented yet")
    }
