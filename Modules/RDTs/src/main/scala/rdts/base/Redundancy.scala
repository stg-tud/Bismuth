package rdts.base

import rdts.time.Dots

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** Decides which deltas in a delta buffer no longer need to be sent once a newer delta is added.
  *
  * This is weaker than lattice subsumption: a buffered delta may be redundant even if `delta` does not subsume it,
  * as long as sending `delta` instead of both does not lose any observable information (e.g., a removal covering an earlier add).
  */
@FunctionalInterface
trait Redundancy[T] {

  /** @param delta the new delta
    * @param bufferedDelta a previously applied delta from the delta buffer
    * @return true if `bufferedDelta` is made redundant by `delta`
    */
  def isRedundant(delta: T, bufferedDelta: T): Boolean
}

object Redundancy {

  inline def apply[T](using redundancy: Redundancy[T]): Redundancy[T] = redundancy

  case class MetaDelta[T](id: Dots, delta: T, redundantDots: Dots) {
    def getAllDots: Dots = id.union(redundantDots)
  }

  object MetaDelta {

    def apply[T](ids: Dots, delta: T): MetaDelta[T] = MetaDelta(ids, delta, Dots.empty)

    extension [T](metaDeltas: Iterable[MetaDelta[T]]) {

      /** @return the dots of all buffered deltas (and the deltas they made redundant) that are made redundant by `delta` */
      def getRedundantDeltas(delta: T)(using redundancy: Redundancy[T]): Dots =
        metaDeltas.foldLeft(Dots.empty) { (dots, bufferedDelta) =>
          if redundancy.isRedundant(delta, bufferedDelta.delta)
          then dots.union(bufferedDelta.getAllDots)
          else dots
        }
    }
  }

  given subsumption[T: Lattice]: Redundancy[T] = (delta, bufferedDelta) => delta `subsumes` bufferedDelta

  inline def derived[T <: Product: Mirror.ProductOf]: Redundancy[T] = productRedundancy[T]

  inline def productRedundancy[T <: Product](using pm: Mirror.ProductOf[T]): Redundancy[T] = {
    val fieldRedundancies: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Redundancy]]
    new ProductRedundancy[T](fieldRedundancies, valueOf[pm.MirroredLabel])
  }

  /** A product delta is redundant if all of its fields are. */
  class ProductRedundancy[T <: Product](fieldRedundancies: Tuple, label: String) extends Redundancy[T] {

    override def toString: String = s"ProductRedundancy[$label]"

    private def field(i: Int): Redundancy[Any] = fieldRedundancies.productElement(i).asInstanceOf[Redundancy[Any]]

    override def isRedundant(delta: T, bufferedDelta: T): Boolean =
      Range(0, fieldRedundancies.productArity).forall { i =>
        field(i).isRedundant(delta.productElement(i), bufferedDelta.productElement(i))
      }
  }

}
