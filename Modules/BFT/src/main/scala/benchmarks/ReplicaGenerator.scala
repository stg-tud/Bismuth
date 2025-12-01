package benchmarks

import datatypes.{Counter, ORSet, Replica}
import scala.util.Random

object ReplicaGenerator:
    def generate[T, R <: Replica[T, R]](size: Int, diff: Float, replica1: R, replica2: R, placeholder: T): (R, R) = {
      var r1  = replica1
      var r2  = replica2
      val rnd = new Random()

      for i <- Range(0, math.round(size * (1 - diff))) do
          if rnd.nextDouble() <= 0.5 then
              r1 = performActionOnReplica(r1, placeholder)
          else {
            r2 = performActionOnReplica(r1, placeholder)
          }

      r1 = r1.merge(r2)
      r2 = r2.merge(r1)

      for i <- Range(0, math.round(size *  diff)) do
          if rnd.nextDouble() <= 0.5 then
              r1 = performActionOnReplica(r1, placeholder)
          else {
            r2 = performActionOnReplica(r1, placeholder)
          }

      (r1, r2)
    }

    def performActionOnReplica[T, R <: Replica[T, R]](r: R, placeholder: T): R =
        val rnd = new Random()
        r match {
          case c @ Counter(hashDAG) => c.merge(c.add(rnd.nextInt()))
          // case s @ ORSet(e, h)      => s.merge(s.add(placeholder))
          case _ => r
        }
